module Brick.Main
  ( App(..)
  , defaultMain
  , customMain
  , simpleMain
  , resizeOrQuit

  -- * Event handler functions
  , continue
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
  )
where

import Control.Exception (finally)
import Lens.Micro ((^.), (&), (.~))
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Concurrent (forkIO, killThread)
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (mempty)
#endif
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
  )
import Graphics.Vty.Attributes (defAttr)

import Brick.BChan (BChan, newBChan, readBChan, readBChan2, writeBChan)
import Brick.Types (Widget, EventM(..))
import Brick.Types.Internal
import Brick.Widgets.Internal
import Brick.AttrMap

-- | The library application abstraction. Your application's operations
-- are represented here and passed to one of the various main functions
-- in this module. An application is in terms of an application state
-- type 's', an application event type 'e', and a resource name type
-- 'n'. In the simplest case 'e' is unused (left polymorphic or set to
-- '()'), but you may define your own event type and use 'customMain'
-- to provide custom events. The state type is the type of application
-- state to be provided by you and iteratively modified by event
-- handlers. The resource name type is the type of names you can assign
-- to rendering resources such as viewports and cursor locations.
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
        -- 'continue', 'suspendAndResume', and 'halt'.
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
    customMain (mkVty defaultConfig) Nothing app st

-- | A simple main entry point which takes a widget and renders it. This
-- event loop terminates when the user presses any key, but terminal
-- resize events cause redraws.
simpleMain :: (Ord n)
           => Widget n
           -- ^ The widget to draw.
           -> IO ()
simpleMain w =
    let app = App { appDraw = const [w]
                  , appHandleEvent = resizeOrQuit
                  , appStartEvent = return
                  , appAttrMap = const $ attrMap defAttr []
                  , appChooseCursor = neverShowCursor
                  }
    in defaultMain app ()

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

runWithNewVty :: (Ord n)
              => Vty
              -> BChan (BrickEvent n e)
              -> Maybe (BChan e)
              -> App s e n
              -> RenderState n
              -> s
              -> IO (InternalNext n s)
runWithNewVty buildVty brickChan mUserChan app initialRS initialSt =
    withVty buildVty $ \vty -> do
        pid <- forkIO $ supplyVtyEvents vty brickChan
        let readEvent = case mUserChan of
              Nothing -> readBChan brickChan
              Just uc -> readBrickEvent brickChan uc
            runInner rs st = do
              (result, newRS) <- runVty vty readEvent app st (rs & observedNamesL .~ S.empty
                                                                 & clickableNamesL .~ mempty)
              case result of
                  SuspendAndResume act -> do
                      killThread pid
                      return $ InternalSuspendAndResume newRS act
                  Halt s -> do
                      killThread pid
                      return $ InternalHalt s
                  Continue s -> runInner newRS s
        runInner initialRS initialSt

-- | The custom event loop entry point to use when the simpler ones
-- don't permit enough control.
customMain :: (Ord n)
           => IO Vty
           -- ^ An IO action to build a Vty handle. This is used to
           -- build a Vty handle whenever the event loop begins or is
           -- resumed after suspension.
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
customMain buildVty mUserChan app initialAppState = do
    initialVty <- buildVty
    let run vty rs st brickChan = do
            result <- runWithNewVty vty brickChan mUserChan app rs st
            case result of
                InternalHalt s -> return s
                InternalSuspendAndResume newRS action -> do
                    newAppState <- action
                    newVty <- buildVty
                    run newVty newRS newAppState brickChan

        emptyES = ES [] []
        eventRO = EventRO M.empty initialVty mempty
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
       -> IO (Next s, RenderState n)
runVty vty readEvent app appState rs = do
    (firstRS, exts) <- renderApp vty app appState rs
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
                (Extent n (Location (ec, er)) _ (Location (oC, oR)):_) ->
                    -- If the clicked extent was registered as
                    -- clickable, send a click event. Otherwise, just
                    -- send the raw mouse event
                    case n `elem` firstRS^.clickableNamesL of
                        True -> do
                            let localCoords = Location (lc, lr)
                                lc = c - ec + oC
                                lr = r - er + oR
                            return (MouseDown n button mods localCoords, firstRS, exts)
                        False -> return (e, firstRS, exts)
                _ -> return (e, firstRS, exts)
        VtyEvent (EvMouseUp c r button) -> do
            let matching = findClickedExtents_ (c, r) exts
            case matching of
                (Extent n (Location (ec, er)) _ (Location (oC, oR)):_) ->
                    -- If the clicked extent was registered as
                    -- clickable, send a click event. Otherwise, just
                    -- send the raw mouse event
                    case n `elem` firstRS^.clickableNamesL of
                        True -> do
                            let localCoords = Location (lc, lr)
                                lc = c - ec + oC
                                lr = r - er + oR
                            return (MouseUp n button localCoords, firstRS, exts)
                        False -> return (e, firstRS, exts)
                _ -> return (e, firstRS, exts)
        _ -> return (e, firstRS, exts)

    let emptyES = ES [] []
        eventRO = EventRO (viewportMap nextRS) vty nextExts

    (next, eState) <- runStateT (runReaderT (runEventM (appHandleEvent app appState e'))
                                eventRO) emptyES
    return (next, nextRS { rsScrollRequests = esScrollRequests eState
                         , renderCache = applyInvalidations (cacheInvalidateRequests eState) $
                                         renderCache nextRS
                         })

applyInvalidations :: (Ord n) => [CacheInvalidateRequest n] -> M.Map n v -> M.Map n v
applyInvalidations ns cache = foldr (.) id (mkFunc <$> ns) cache
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
clickedExtent (c, r) (Extent _ (Location (lc, lr)) (w, h) _) =
   c >= lc && c < (lc + w) &&
   r >= lr && r < (lr + h)

-- | Given a resource name, get the most recent rendering extent for the
-- name (if any).
lookupExtent :: (Eq n) => n -> EventM n (Maybe (Extent n))
lookupExtent n = EventM $ asks (listToMaybe . filter f . latestExtents)
    where
        f (Extent n' _ _ _) = n == n'

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
invalidateCacheEntry :: n -> EventM n ()
invalidateCacheEntry n = EventM $ do
    lift $ modify (\s -> s { cacheInvalidateRequests = InvalidateSingle n : cacheInvalidateRequests s })

-- | Invalidate the entire rendering cache.
invalidateCache :: EventM n ()
invalidateCache = EventM $ do
    lift $ modify (\s -> s { cacheInvalidateRequests = InvalidateEntire : cacheInvalidateRequests s })

withVty :: Vty -> (Vty -> IO a) -> IO a
withVty vty useVty = do
    useVty vty `finally` shutdown vty

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
            Just cloc -> pic { picCursor = AbsoluteCursor (cloc^.locationColumnL)
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

-- | Halt the event loop and return the specified application state as
-- the final state value.
halt :: s -> EventM n (Next s)
halt = return . Halt

-- | Suspend the event loop, save the terminal state, and run the
-- specified action. When it returns an application state value, restore
-- the terminal state, redraw the application from the new state, and
-- resume the event loop.
suspendAndResume :: IO s -> EventM n (Next s)
suspendAndResume = return . SuspendAndResume
