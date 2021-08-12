{-# LANGUAGE ScopedTypeVariables #-}
module Brick.Main
  ( App(..)
  , defaultMain
  , customMain
  , customMainWithVty
  , simpleMain
  , resizeOrQuit
  , simpleApp

  -- * Event handler functions
  , continue
  , continueWithoutRedraw
  , halt
  , suspendAndResume
  , lookupViewport
  , lookupExtent
  , findClickedExtents
  , clickedExtent
  , getVtyHandle

  -- ** Viewport scrolling
  , viewportScroll
  , ViewportScroll
  , vScrollBy
  , vScrollPage
  , vScrollToBeginning
  , vScrollToEnd
  , hScrollBy
  , hScrollPage
  , hScrollToBeginning
  , hScrollToEnd
  , setTop
  , setLeft

  -- * Cursor management functions
  , neverShowCursor
  , showFirstCursor
  , showCursorNamed

  -- * Rendering cache management
  , invalidateCacheEntry
  , invalidateCache

  -- * Renderer internals (for benchmarking)
  , renderFinal
  , getRenderState
  , resetRenderState
  )
where

import qualified Control.Exception as E
import Lens.Micro ((^.), (&), (.~), (%~), _1, _2)
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Concurrent (forkIO, killThread)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (mempty)
#endif
import qualified Data.Foldable as F
import Data.Maybe (listToMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import Graphics.Vty
  ( Vty
  , Picture(..)
  , Cursor(..)
  , Event(..)
  , update
  , outputIface
  , displayBounds
  , shutdown
  , nextEvent
  , mkVty
  , defaultConfig
  , restoreInputState
  , inputIface
  )
import Graphics.Vty.Attributes (defAttr)

import Brick.BChan (BChan, newBChan, readBChan, readBChan2, writeBChan)
import Brick.Types (Widget, EventM(..))
import Brick.Types.Internal
import Brick.Widgets.Internal
import Brick.AttrMap

-- | The library application abstraction. Your application's operations
-- are provided in an @App@ and then the @App@ is provided to one of the
-- various main functions in this module. An application @App s e n@
-- is in terms of an application state type @s@, an application event
-- type @e@, and a resource name type @n@. In the simplest case 'e' is
-- unused (left polymorphic or set to @()@), but you may define your own
-- event type and use 'customMain' to provide custom events. The state
-- type @s@ is the type of application state to be provided by you and
-- iteratively modified by event handlers. The resource name type @n@
-- is the type of names you can assign to rendering resources such as
-- viewports and cursor locations. Your application must define this
-- type.
data App s e n =
    App { appDraw :: s -> [Widget n]
        -- ^ This function turns your application state into a list of
        -- widget layers. The layers are listed topmost first.
        , appChooseCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
        -- ^ This function chooses which of the zero or more cursor
        -- locations reported by the rendering process should be
        -- selected as the one to use to place the cursor. If this
        -- returns 'Nothing', no cursor is placed. The rationale here
        -- is that many widgets may request a cursor placement but your
        -- application state is what you probably want to use to decide
        -- which one wins.
        , appHandleEvent :: s -> BrickEvent n e -> EventM n (Next s)
        -- ^ This function takes the current application state and an
        -- event and returns an action to be taken and a corresponding
        -- transformed application state. Possible options are
        -- 'continue', 'continueWithoutRedraw', 'suspendAndResume', and
        -- 'halt'.
        , appStartEvent :: s -> EventM n s
        -- ^ This function gets called once just prior to the first
        -- drawing of your application. Here is where you can make
        -- initial scrolling requests, for example.
        , appAttrMap :: s -> AttrMap
        -- ^ The attribute map that should be used during rendering.
        }

-- | The default main entry point which takes an application and an
-- initial state and returns the final state returned by a 'halt'
-- operation.
defaultMain :: (Ord n)
            => App s e n
            -- ^ The application.
            -> s
            -- ^ The initial application state.
            -> IO s
defaultMain app st = do
    let builder = mkVty defaultConfig
    initialVty <- builder
    customMain initialVty builder Nothing app st

-- | A simple main entry point which takes a widget and renders it. This
-- event loop terminates when the user presses any key, but terminal
-- resize events cause redraws.
simpleMain :: (Ord n)
           => Widget n
           -- ^ The widget to draw.
           -> IO ()
simpleMain w = defaultMain (simpleApp w) ()

-- | A simple application with reasonable defaults to be overridden as
-- desired:
--
-- * Draws only the specified widget
-- * Quits on any event other than resizes
-- * Has no start event handler
-- * Provides no attribute map
-- * Never shows any cursors
simpleApp :: Widget n -> App s e n
simpleApp w =
    App { appDraw = const [w]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const $ attrMap defAttr []
        , appChooseCursor = neverShowCursor
        }

-- | An event-handling function which continues execution of the event
-- loop only when resize events occur; all other types of events trigger
-- a halt. This is a convenience function useful as an 'appHandleEvent'
-- value for simple applications using the 'Event' type that do not need
-- to get more sophisticated user input.
resizeOrQuit :: s -> BrickEvent n e -> EventM n (Next s)
resizeOrQuit s (VtyEvent (EvResize _ _)) = continue s
resizeOrQuit s _ = halt s

data InternalNext n a = InternalSuspendAndResume (RenderState n) (IO a)
                      | InternalHalt a

readBrickEvent :: BChan (BrickEvent n e) -> BChan e -> IO (BrickEvent n e)
readBrickEvent brickChan userChan = either id AppEvent <$> readBChan2 brickChan userChan

runWithVty :: (Ord n)
           => Vty
           -> BChan (BrickEvent n e)
           -> Maybe (BChan e)
           -> App s e n
           -> RenderState n
           -> s
           -> IO (InternalNext n s)
runWithVty vty brickChan mUserChan app initialRS initialSt = do
    pid <- forkIO $ supplyVtyEvents vty brickChan
    let readEvent = case mUserChan of
          Nothing -> readBChan brickChan
          Just uc -> readBrickEvent brickChan uc
        runInner rs es draw st = do
          (result, newRS, newExtents) <- runVty vty readEvent app st (resetRenderState rs) es draw
          case result of
              SuspendAndResume act -> do
                  killThread pid
                  return $ InternalSuspendAndResume newRS act
              Halt s -> do
                  killThread pid
                  return $ InternalHalt s
              Continue s -> runInner newRS newExtents True s
              ContinueWithoutRedraw s ->
                  runInner newRS newExtents False s
    runInner initialRS mempty True initialSt

-- | The custom event loop entry point to use when the simpler ones
-- don't permit enough control. Returns the final application state
-- after the application halts.
--
-- Note that this function guarantees that the terminal input state
-- prior to the first Vty initialization is the terminal input state
-- that is restored on shutdown (regardless of exceptions).
customMain :: (Ord n)
           => Vty
           -- ^ The initial Vty handle to use.
           -> IO Vty
           -- ^ An IO action to build a Vty handle. This is used
           -- to build a Vty handle whenever the event loop needs
           -- to reinitialize the terminal, e.g. on resume after
           -- suspension.
           -> Maybe (BChan e)
           -- ^ An event channel for sending custom events to the event
           -- loop (you write to this channel, the event loop reads from
           -- it). Provide 'Nothing' if you don't plan on sending custom
           -- events.
           -> App s e n
           -- ^ The application.
           -> s
           -- ^ The initial application state.
           -> IO s
customMain initialVty buildVty mUserChan app initialAppState = do
    let restoreInitialState = restoreInputState $ inputIface initialVty

    (s, vty) <- customMainWithVty initialVty buildVty mUserChan app initialAppState
        `E.catch` (\(e::E.SomeException) -> restoreInitialState >> E.throw e)

    shutdown vty
    restoreInitialState
    return s

-- | Like 'customMain', except the last 'Vty' handle used by the
-- application is returned without being shut down with 'shutdown'. This
-- allows the caller to re-use the 'Vty' handle for something else, such
-- as another Brick application.
customMainWithVty :: (Ord n)
                  => Vty
                  -- ^ The initial Vty handle to use.
                  -> IO Vty
                  -- ^ An IO action to build a Vty handle. This is used
                  -- to build a Vty handle whenever the event loop needs
                  -- to reinitialize the terminal, e.g. on resume after
                  -- suspension.
                  -> Maybe (BChan e)
                  -- ^ An event channel for sending custom events to the event
                  -- loop (you write to this channel, the event loop reads from
                  -- it). Provide 'Nothing' if you don't plan on sending custom
                  -- events.
                  -> App s e n
                  -- ^ The application.
                  -> s
                  -- ^ The initial application state.
                  -> IO (s, Vty)
customMainWithVty initialVty buildVty mUserChan app initialAppState = do
    let run vty rs st brickChan = do
            result <- runWithVty vty brickChan mUserChan app rs st
                `E.catch` (\(e::E.SomeException) -> shutdown vty >> E.throw e)
            case result of
                InternalHalt s -> return (s, vty)
                InternalSuspendAndResume newRS action -> do
                    shutdown vty
                    newAppState <- action
                    newVty <- buildVty
                    run newVty (newRS { renderCache = mempty }) newAppState brickChan

    let emptyES = ES [] mempty
        emptyRS = RS M.empty mempty S.empty mempty mempty
        eventRO = EventRO M.empty initialVty mempty emptyRS

    (st, eState) <- runStateT (runReaderT (runEventM (appStartEvent app initialAppState)) eventRO) emptyES
    let initialRS = RS M.empty (esScrollRequests eState) S.empty mempty []
    brickChan <- newBChan 20
    run initialVty initialRS st brickChan

supplyVtyEvents :: Vty -> BChan (BrickEvent n e) -> IO ()
supplyVtyEvents vty chan =
    forever $ do
        e <- nextEvent vty
        writeBChan chan $ VtyEvent e

runVty :: (Ord n)
       => Vty
       -> IO (BrickEvent n e)
       -> App s e n
       -> s
       -> RenderState n
       -> [Extent n]
       -> Bool
       -> IO (Next s, RenderState n, [Extent n])
runVty vty readEvent app appState rs prevExtents draw = do
    (firstRS, exts) <- case draw of
        True -> renderApp vty app appState rs
        False -> return (rs, prevExtents)

    e <- readEvent

    (e', nextRS, nextExts) <- case e of
        -- If the event was a resize, redraw the UI to update the
        -- viewport states before we invoke the event handler since we
        -- want the event handler to have access to accurate viewport
        -- information.
        VtyEvent (EvResize _ _) -> do
            (rs', exts') <- renderApp vty app appState $ firstRS & observedNamesL .~ S.empty
            return (e, rs', exts')
        VtyEvent (EvMouseDown c r button mods) -> do
            let matching = findClickedExtents_ (c, r) exts
            case matching of
                (Extent n (Location (ec, er)) _:_) ->
                    -- If the clicked extent was registered as
                    -- clickable, send a click event. Otherwise, just
                    -- send the raw mouse event
                    case n `elem` firstRS^.clickableNamesL of
                        True -> do
                            let localCoords = Location (lc, lr)
                                lc = c - ec
                                lr = r - er

                                -- If the clicked extent was a viewport,
                                -- adjust the local coordinates by
                                -- adding the viewport upper-left corner
                                -- offset.
                                newCoords = case M.lookup n (viewportMap firstRS) of
                                  Nothing -> localCoords
                                  Just vp -> localCoords & _1 %~ (+ (vp^.vpLeft))
                                                         & _2 %~ (+ (vp^.vpTop))

                            return (MouseDown n button mods newCoords, firstRS, exts)
                        False -> return (e, firstRS, exts)
                _ -> return (e, firstRS, exts)
        VtyEvent (EvMouseUp c r button) -> do
            let matching = findClickedExtents_ (c, r) exts
            case matching of
                (Extent n (Location (ec, er)) _:_) ->
                    -- If the clicked extent was registered as
                    -- clickable, send a click event. Otherwise, just
                    -- send the raw mouse event
                    case n `elem` firstRS^.clickableNamesL of
                        True -> do
                            let localCoords = Location (lc, lr)
                                lc = c - ec
                                lr = r - er
                                -- If the clicked extent was a viewport,
                                -- adjust the local coordinates by
                                -- adding the viewport upper-left corner
                                -- offset.
                                newCoords = case M.lookup n (viewportMap firstRS) of
                                  Nothing -> localCoords
                                  Just vp -> localCoords & _1 %~ (+ (vp^.vpLeft))
                                                         & _2 %~ (+ (vp^.vpTop))
                            return (MouseUp n button newCoords, firstRS, exts)
                        False -> return (e, firstRS, exts)
                _ -> return (e, firstRS, exts)
        _ -> return (e, firstRS, exts)

    let emptyES = ES [] mempty
        eventRO = EventRO (viewportMap nextRS) vty nextExts nextRS

    (next, eState) <- runStateT (runReaderT (runEventM (appHandleEvent app appState e'))
                                eventRO) emptyES
    return ( next
           , nextRS { rsScrollRequests = esScrollRequests eState
                    , renderCache = applyInvalidations (cacheInvalidateRequests eState) $
                                    renderCache nextRS
                    }
           , nextExts
           )

applyInvalidations :: (Ord n) => S.Set (CacheInvalidateRequest n) -> M.Map n v -> M.Map n v
applyInvalidations ns cache =
    if InvalidateEntire `S.member` ns
    then mempty
    else foldr (.) id (mkFunc <$> F.toList ns) cache
    where
        mkFunc InvalidateEntire = const mempty
        mkFunc (InvalidateSingle n) = M.delete n

-- | Given a viewport name, get the viewport's size and offset
-- information from the most recent rendering. Returns 'Nothing' if
-- no such state could be found, either because the name was invalid
-- or because no rendering has occurred (e.g. in an 'appStartEvent'
-- handler).
lookupViewport :: (Ord n) => n -> EventM n (Maybe Viewport)
lookupViewport n = EventM $ asks (M.lookup n . eventViewportMap)

-- | Did the specified mouse coordinates (column, row) intersect the
-- specified extent?
clickedExtent :: (Int, Int) -> Extent n -> Bool
clickedExtent (c, r) (Extent _ (Location (lc, lr)) (w, h)) =
   c >= lc && c < (lc + w) &&
   r >= lr && r < (lr + h)

-- | Given a resource name, get the most recent rendering extent for the
-- name (if any).
lookupExtent :: (Eq n) => n -> EventM n (Maybe (Extent n))
lookupExtent n = EventM $ asks (listToMaybe . filter f . latestExtents)
    where
        f (Extent n' _ _) = n == n'

-- | Given a mouse click location, return the extents intersected by the
-- click. The returned extents are sorted such that the first extent in
-- the list is the most specific extent and the last extent is the most
-- generic (top-level). So if two extents A and B both intersected the
-- mouse click but A contains B, then they would be returned [B, A].
findClickedExtents :: (Int, Int) -> EventM n [Extent n]
findClickedExtents pos = EventM $ asks (findClickedExtents_ pos . latestExtents)

findClickedExtents_ :: (Int, Int) -> [Extent n] -> [Extent n]
findClickedExtents_ pos = reverse . filter (clickedExtent pos)

-- | Get the Vty handle currently in use.
getVtyHandle :: EventM n Vty
getVtyHandle = EventM $ asks eventVtyHandle

-- | Invalidate the rendering cache entry with the specified resource
-- name.
invalidateCacheEntry :: (Ord n) => n -> EventM n ()
invalidateCacheEntry n = EventM $ do
    lift $ modify (\s -> s { cacheInvalidateRequests = S.insert (InvalidateSingle n) $ cacheInvalidateRequests s })

-- | Invalidate the entire rendering cache.
invalidateCache :: (Ord n) => EventM n ()
invalidateCache = EventM $ do
    lift $ modify (\s -> s { cacheInvalidateRequests = S.insert InvalidateEntire $ cacheInvalidateRequests s })

getRenderState :: EventM n (RenderState n)
getRenderState = EventM $ asks oldState

resetRenderState :: RenderState n -> RenderState n
resetRenderState s =
    s & observedNamesL .~ S.empty
      & clickableNamesL .~ mempty

renderApp :: Vty -> App s e n -> s -> RenderState n -> IO (RenderState n, [Extent n])
renderApp vty app appState rs = do
    sz <- displayBounds $ outputIface vty
    let (newRS, pic, theCursor, exts) = renderFinal (appAttrMap app appState)
                                        (appDraw app appState)
                                        sz
                                        (appChooseCursor app appState)
                                        rs
        picWithCursor = case theCursor of
            Nothing -> pic { picCursor = NoCursor }
            Just cloc -> pic { picCursor = (if cursorLocationVisible cloc
                                            then AbsoluteCursor
                                            else PositionOnly True)
                                           (cloc^.locationColumnL)
                                           (cloc^.locationRowL)
                             }

    update vty picWithCursor

    return (newRS, exts)

-- | Ignore all requested cursor positions returned by the rendering
-- process. This is a convenience function useful as an
-- 'appChooseCursor' value when a simple application has no need to
-- position the cursor.
neverShowCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
neverShowCursor = const $ const Nothing

-- | Always show the first cursor, if any, returned by the rendering
-- process. This is a convenience function useful as an
-- 'appChooseCursor' value when a simple program has zero or more
-- widgets that advertise a cursor position.
showFirstCursor :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
showFirstCursor = const listToMaybe

-- | Show the cursor with the specified resource name, if such a cursor
-- location has been reported.
showCursorNamed :: (Eq n) => n -> [CursorLocation n] -> Maybe (CursorLocation n)
showCursorNamed name locs =
    let matches l = l^.cursorLocationNameL == Just name
    in listToMaybe $ filter matches locs

-- | A viewport scrolling handle for managing the scroll state of
-- viewports.
data ViewportScroll n =
    ViewportScroll { viewportName :: n
                   -- ^ The name of the viewport to be controlled by
                   -- this scrolling handle.
                   , hScrollPage :: Direction -> EventM n ()
                   -- ^ Scroll the viewport horizontally by one page in
                   -- the specified direction.
                   , hScrollBy :: Int -> EventM n ()
                   -- ^ Scroll the viewport horizontally by the
                   -- specified number of rows or columns depending on
                   -- the orientation of the viewport.
                   , hScrollToBeginning :: EventM n ()
                   -- ^ Scroll horizontally to the beginning of the
                   -- viewport.
                   , hScrollToEnd :: EventM n ()
                   -- ^ Scroll horizontally to the end of the viewport.
                   , vScrollPage :: Direction -> EventM n ()
                   -- ^ Scroll the viewport vertically by one page in
                   -- the specified direction.
                   , vScrollBy :: Int -> EventM n ()
                   -- ^ Scroll the viewport vertically by the specified
                   -- number of rows or columns depending on the
                   -- orientation of the viewport.
                   , vScrollToBeginning :: EventM n ()
                   -- ^ Scroll vertically to the beginning of the viewport.
                   , vScrollToEnd :: EventM n ()
                   -- ^ Scroll vertically to the end of the viewport.
                   , setTop :: Int -> EventM n ()
                   -- ^ Set the top row offset of the viewport.
                   , setLeft :: Int -> EventM n ()
                   -- ^ Set the left column offset of the viewport.
                   }

addScrollRequest :: (n, ScrollRequest) -> EventM n ()
addScrollRequest req = EventM $ do
    lift $ modify (\s -> s { esScrollRequests = req : esScrollRequests s })

-- | Build a viewport scroller for the viewport with the specified name.
viewportScroll :: n -> ViewportScroll n
viewportScroll n =
    ViewportScroll { viewportName       = n
                   , hScrollPage        = \dir -> addScrollRequest (n, HScrollPage dir)
                   , hScrollBy          = \i ->   addScrollRequest (n, HScrollBy i)
                   , hScrollToBeginning =         addScrollRequest (n, HScrollToBeginning)
                   , hScrollToEnd       =         addScrollRequest (n, HScrollToEnd)
                   , vScrollPage        = \dir -> addScrollRequest (n, VScrollPage dir)
                   , vScrollBy          = \i ->   addScrollRequest (n, VScrollBy i)
                   , vScrollToBeginning =         addScrollRequest (n, VScrollToBeginning)
                   , vScrollToEnd       =         addScrollRequest (n, VScrollToEnd)
                   , setTop             = \i ->   addScrollRequest (n, SetTop i)
                   , setLeft            = \i ->   addScrollRequest (n, SetLeft i)
                   }

-- | Continue running the event loop with the specified application
-- state.
continue :: s -> EventM n (Next s)
continue = return . Continue

-- | Continue running the event loop with the specified application
-- state without redrawing the screen. This is faster than 'continue'
-- because it skips the redraw, but the drawback is that you need to
-- be really sure that you don't want a screen redraw. If your state
-- changed in a way that needs to be reflected on the screen, use
-- 'continue'. This function is for cases where you know that you did
-- something that won't have an impact on the screen state and you want
-- to save on redraw cost.
continueWithoutRedraw :: s -> EventM n (Next s)
continueWithoutRedraw = return . ContinueWithoutRedraw

-- | Halt the event loop and return the specified application state as
-- the final state value.
halt :: s -> EventM n (Next s)
halt = return . Halt

-- | Suspend the event loop, save the terminal state, and run the
-- specified action. When it returns an application state value, restore
-- the terminal state, empty the rendering cache, redraw the application
-- from the new state, and resume the event loop.
--
-- Note that any changes made to the terminal's input state are ignored
-- when Brick resumes execution and are not preserved in the final
-- terminal input state after the Brick application returns the terminal
-- to the user.
suspendAndResume :: IO s -> EventM n (Next s)
suspendAndResume = return . SuspendAndResume
