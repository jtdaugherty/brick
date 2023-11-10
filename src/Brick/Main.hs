{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Brick.Main
  ( App(..)
  , defaultMain
  , customMain
  , customMainWithVty
  , customMainWithDefaultVty
  , simpleMain
  , resizeOrQuit
  , simpleApp

  -- * Event handler functions
  , continueWithoutRedraw
  , halt
  , suspendAndResume
  , suspendAndResume'
  , makeVisible
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
  , renderWidget
  )
where

import qualified Control.Exception as E
import Lens.Micro ((^.), (&), (.~), (%~), _1, _2)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import Control.Concurrent (forkIO, killThread)
import qualified Data.Foldable as F
import Data.List (find)
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
  , restoreInputState
  , inputIface
  )
import Graphics.Vty.CrossPlatform (mkVty)
import Graphics.Vty.Config (defaultConfig)
import Graphics.Vty.Attributes (defAttr)

import Brick.BChan (BChan, newBChan, readBChan, readBChan2, writeBChan)
import Brick.Types.EventM
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
        , appHandleEvent :: BrickEvent n e -> EventM n s ()
        -- ^ This function handles an event and updates the current
        -- application state.
        , appStartEvent :: EventM n s ()
        -- ^ This function gets called once just prior to the first
        -- drawing of your application. Here is where you can make
        -- initial scrolling requests, for example.
        , appAttrMap :: s -> AttrMap
        -- ^ The attribute map that should be used during rendering.
        }

-- | The default main entry point which takes an application and an
-- initial state and returns the final state from 'EventM' once the
-- program exits.
defaultMain :: (Ord n)
            => App s e n
            -- ^ The application.
            -> s
            -- ^ The initial application state.
            -> IO s
defaultMain app st = do
    (s, vty) <- customMainWithDefaultVty Nothing app st
    shutdown vty
    return s

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
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        , appChooseCursor = neverShowCursor
        }

-- | An event-handling function which continues execution of the event
-- loop only when resize events occur; all other types of events trigger
-- a halt. This is a convenience function useful as an 'appHandleEvent'
-- value for simple applications using the 'Event' type that do not need
-- to get more sophisticated user input.
resizeOrQuit :: BrickEvent n e -> EventM n s ()
resizeOrQuit (VtyEvent (EvResize _ _)) = return ()
resizeOrQuit _ = halt

readBrickEvent :: BChan (BrickEvent n e) -> BChan e -> IO (BrickEvent n e)
readBrickEvent brickChan userChan = either id AppEvent <$> readBChan2 brickChan userChan

runWithVty :: (Ord n)
           => VtyContext
           -> BChan (BrickEvent n e)
           -> Maybe (BChan e)
           -> App s e n
           -> RenderState n
           -> s
           -> IO (s, VtyContext)
runWithVty vtyCtx brickChan mUserChan app initialRS initialSt = do
    let readEvent = case mUserChan of
          Nothing -> readBChan brickChan
          Just uc -> readBrickEvent brickChan uc
        runInner ctx rs es draw st = do
          let nextRS = if draw
                       then resetRenderState rs
                       else rs
          (nextSt, result, newRS, newExtents, newCtx) <- runVty ctx readEvent app st nextRS es draw
          case result of
              Halt ->
                  return (nextSt, newCtx)
              Continue ->
                  runInner newCtx newRS newExtents True nextSt
              ContinueWithoutRedraw ->
                  runInner newCtx newRS newExtents False nextSt

    runInner vtyCtx initialRS mempty True initialSt

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

-- | Like 'customMainWithVty', except that Vty is initialized with the
-- default configuration.
customMainWithDefaultVty :: (Ord n)
                         => Maybe (BChan e)
                         -- ^ An event channel for sending custom
                         -- events to the event loop (you write to this
                         -- channel, the event loop reads from it).
                         -- Provide 'Nothing' if you don't plan on
                         -- sending custom events.
                         -> App s e n
                         -- ^ The application.
                         -> s
                         -- ^ The initial application state.
                         -> IO (s, Vty)
customMainWithDefaultVty mUserChan app initialAppState = do
    let builder = mkVty defaultConfig
    vty <- builder
    customMainWithVty vty builder mUserChan app initialAppState

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
    brickChan <- newBChan 20
    vtyCtx <- newVtyContext buildVty (Just initialVty) (writeBChan brickChan . VtyEvent)

    let emptyES = ES { esScrollRequests = []
                     , cacheInvalidateRequests = mempty
                     , requestedVisibleNames = mempty
                     , nextAction = Continue
                     , vtyContext = vtyCtx
                     }
        emptyRS = RS M.empty mempty S.empty mempty mempty mempty mempty
        eventRO = EventRO M.empty mempty emptyRS

    (((), appState), eState) <- runStateT (runStateT (runReaderT (runEventM (appStartEvent app)) eventRO) initialAppState) emptyES
    let initialRS = RS { viewportMap = M.empty
                       , rsScrollRequests = esScrollRequests eState
                       , observedNames = S.empty
                       , renderCache = mempty
                       , clickableNames = []
                       , requestedVisibleNames_ = requestedVisibleNames eState
                       , reportedExtents = mempty
                       }

    (s, ctx) <- runWithVty vtyCtx brickChan mUserChan app initialRS appState
                `E.catch` (\(e::E.SomeException) -> shutdownVtyContext vtyCtx >> E.throw e)

    -- Shut down the context's event thread but do NOT shut down Vty
    -- itself because we want the handle to be live when we return it to
    -- the caller.
    shutdownVtyContextThread ctx
    return (s, vtyContextHandle ctx)

supplyVtyEvents :: Vty -> (Event -> IO ()) -> IO ()
supplyVtyEvents vty putEvent =
    forever $ putEvent =<< nextEvent vty

newVtyContextFrom :: VtyContext -> IO VtyContext
newVtyContextFrom old =
    newVtyContext (vtyContextBuilder old) Nothing (vtyContextPutEvent old)

newVtyContext :: IO Vty -> Maybe Vty -> (Event -> IO ()) -> IO VtyContext
newVtyContext builder handle putEvent = do
    vty <- case handle of
        Just h -> return h
        Nothing -> builder
    tId <- forkIO $ supplyVtyEvents vty putEvent
    return VtyContext { vtyContextHandle = vty
                      , vtyContextBuilder = builder
                      , vtyContextThread = tId
                      , vtyContextPutEvent = putEvent
                      }

shutdownVtyContext :: VtyContext -> IO ()
shutdownVtyContext ctx = do
    shutdown $ vtyContextHandle ctx
    shutdownVtyContextThread ctx

shutdownVtyContextThread :: VtyContext -> IO ()
shutdownVtyContextThread ctx =
    killThread $ vtyContextThread ctx

runVty :: (Ord n)
       => VtyContext
       -> IO (BrickEvent n e)
       -> App s e n
       -> s
       -> RenderState n
       -> [Extent n]
       -> Bool
       -> IO (s, NextAction, RenderState n, [Extent n], VtyContext)
runVty vtyCtx readEvent app appState rs prevExtents draw = do
    (firstRS, exts) <- if draw
                       then renderApp vtyCtx app appState rs
                       else return (rs, prevExtents)

    e <- readEvent

    (e', nextRS, nextExts) <- case e of
        -- If the event was a resize, redraw the UI to update the
        -- viewport states before we invoke the event handler since we
        -- want the event handler to have access to accurate viewport
        -- information.
        VtyEvent (EvResize _ _) -> do
            (rs', exts') <- renderApp vtyCtx app appState $ firstRS & observedNamesL .~ S.empty
            return (e, rs', exts')
        VtyEvent (EvMouseDown c r button mods) -> do
            let matching = findClickedExtents_ (c, r) exts
            case matching of
                (Extent n (Location (ec, er)) _:_) ->
                    -- If the clicked extent was registered as
                    -- clickable, send a click event. Otherwise, just
                    -- send the raw mouse event
                    if n `elem` firstRS^.clickableNamesL
                    then do
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
                    else return (e, firstRS, exts)
                _ -> return (e, firstRS, exts)
        VtyEvent (EvMouseUp c r button) -> do
            let matching = findClickedExtents_ (c, r) exts
            case matching of
                (Extent n (Location (ec, er)) _:_) ->
                    -- If the clicked extent was registered as
                    -- clickable, send a click event. Otherwise, just
                    -- send the raw mouse event
                    if n `elem` firstRS^.clickableNamesL
                    then do
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
                    else return (e, firstRS, exts)
                _ -> return (e, firstRS, exts)
        _ -> return (e, firstRS, exts)

    let emptyES = ES [] mempty mempty Continue vtyCtx
        eventRO = EventRO (viewportMap nextRS) nextExts nextRS

    (((), newAppState), eState) <- runStateT (runStateT (runReaderT (runEventM (appHandleEvent app e'))
                                eventRO) appState) emptyES
    return ( newAppState
           , nextAction eState
           , nextRS { rsScrollRequests = esScrollRequests eState
                    , renderCache = applyInvalidations (cacheInvalidateRequests eState) $
                                    renderCache nextRS
                    , requestedVisibleNames_ = requestedVisibleNames eState
                    }
           , nextExts
           , vtyContext eState
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
-- handler). An important consequence of this behavior is that if this
-- function is called before a viewport is rendered for the first
-- time, no state will be found because the renderer only knows about
-- viewports it has rendered in the most recent rendering. As a result,
-- if you need to make viewport transformations before they are drawn
-- for the first time, you may need to use 'viewportScroll' and its
-- associated functions without relying on this function. Those
-- functions queue up scrolling requests that can be made in advance of
-- the next rendering to affect the viewport.
lookupViewport :: (Ord n) => n -> EventM n s (Maybe Viewport)
lookupViewport n = EventM $ asks (M.lookup n . eventViewportMap)

-- | Did the specified mouse coordinates (column, row) intersect the
-- specified extent?
clickedExtent :: (Int, Int) -> Extent n -> Bool
clickedExtent (c, r) (Extent _ (Location (lc, lr)) (w, h)) =
   c >= lc && c < (lc + w) &&
   r >= lr && r < (lr + h)

-- | Given a resource name, get the most recent rendering extent for the
-- name (if any).
lookupExtent :: (Eq n) => n -> EventM n s (Maybe (Extent n))
lookupExtent n = EventM $ asks (find f . latestExtents)
    where
        f (Extent n' _ _) = n == n'

-- | Given a mouse click location, return the extents intersected by the
-- click. The returned extents are sorted such that the first extent in
-- the list is the most specific extent and the last extent is the most
-- generic (top-level). So if two extents A and B both intersected the
-- mouse click but A contains B, then they would be returned [B, A].
findClickedExtents :: (Int, Int) -> EventM n s [Extent n]
findClickedExtents pos = EventM $ asks (findClickedExtents_ pos . latestExtents)

findClickedExtents_ :: (Int, Int) -> [Extent n] -> [Extent n]
findClickedExtents_ pos = reverse . filter (clickedExtent pos)

-- | Get the Vty handle currently in use.
getVtyHandle :: EventM n s Vty
getVtyHandle = vtyContextHandle <$> getVtyContext

setVtyContext :: VtyContext -> EventM n s ()
setVtyContext ctx =
    EventM $ lift $ lift $ modify $ \s -> s { vtyContext = ctx }

-- | Invalidate the rendering cache entry with the specified resource
-- name.
invalidateCacheEntry :: (Ord n) => n -> EventM n s ()
invalidateCacheEntry n = EventM $ do
    lift $ lift $ modify (\s -> s { cacheInvalidateRequests = S.insert (InvalidateSingle n) $ cacheInvalidateRequests s })

-- | Invalidate the entire rendering cache.
invalidateCache :: (Ord n) => EventM n s ()
invalidateCache = EventM $ do
    lift $ lift $ modify (\s -> s { cacheInvalidateRequests = S.insert InvalidateEntire $ cacheInvalidateRequests s })

getRenderState :: EventM n s (RenderState n)
getRenderState = EventM $ asks oldState

resetRenderState :: RenderState n -> RenderState n
resetRenderState s =
    s & observedNamesL .~ S.empty
      & clickableNamesL .~ mempty

renderApp :: (Ord n) => VtyContext -> App s e n -> s -> RenderState n -> IO (RenderState n, [Extent n])
renderApp vtyCtx app appState rs = do
    sz <- displayBounds $ outputIface $ vtyContextHandle vtyCtx
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

    update (vtyContextHandle vtyCtx) picWithCursor

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
    in find matches locs

-- | A viewport scrolling handle for managing the scroll state of
-- viewports.
data ViewportScroll n =
    ViewportScroll { viewportName :: n
                   -- ^ The name of the viewport to be controlled by
                   -- this scrolling handle.
                   , hScrollPage :: forall s. Direction -> EventM n s ()
                   -- ^ Scroll the viewport horizontally by one page in
                   -- the specified direction.
                   , hScrollBy :: forall s. Int -> EventM n s ()
                   -- ^ Scroll the viewport horizontally by the
                   -- specified number of rows or columns depending on
                   -- the orientation of the viewport.
                   , hScrollToBeginning :: forall s. EventM n s ()
                   -- ^ Scroll horizontally to the beginning of the
                   -- viewport.
                   , hScrollToEnd :: forall s. EventM n s ()
                   -- ^ Scroll horizontally to the end of the viewport.
                   , vScrollPage :: forall s. Direction -> EventM n s ()
                   -- ^ Scroll the viewport vertically by one page in
                   -- the specified direction.
                   , vScrollBy :: forall s. Int -> EventM n s ()
                   -- ^ Scroll the viewport vertically by the specified
                   -- number of rows or columns depending on the
                   -- orientation of the viewport.
                   , vScrollToBeginning :: forall s. EventM n s ()
                   -- ^ Scroll vertically to the beginning of the viewport.
                   , vScrollToEnd :: forall s. EventM n s ()
                   -- ^ Scroll vertically to the end of the viewport.
                   , setTop :: forall s. Int -> EventM n s ()
                   -- ^ Set the top row offset of the viewport.
                   , setLeft :: forall s. Int -> EventM n s ()
                   -- ^ Set the left column offset of the viewport.
                   }

addScrollRequest :: (n, ScrollRequest) -> EventM n s ()
addScrollRequest req = EventM $ do
    lift $ lift $ modify (\s -> s { esScrollRequests = req : esScrollRequests s })

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
-- state without redrawing the screen. This is faster than 'continue'
-- because it skips the redraw, but the drawback is that you need to
-- be really sure that you don't want a screen redraw. If your state
-- changed in a way that needs to be reflected on the screen, just don't
-- call this; 'EventM' blocks default to triggering redraws when they
-- finish executing. This function is for cases where you know that you
-- did something that won't have an impact on the screen state and you
-- want to save on redraw cost.
continueWithoutRedraw :: EventM n s ()
continueWithoutRedraw =
    EventM $ lift $ lift $ modify $ \es -> es { nextAction = ContinueWithoutRedraw }

-- | Halt the event loop and return the specified application state as
-- the final state value.
halt :: EventM n s ()
halt =
    EventM $ lift $ lift $ modify $ \es -> es { nextAction = Halt }

-- | Suspend the event loop, save the terminal state, and run the
-- specified action. When it returns an application state value, restore
-- the terminal state, empty the rendering cache, update the application
-- state with the returned state, and continue execution of the event
-- handler that called this.
--
-- Note that any changes made to the terminal's input state are ignored
-- when Brick resumes execution and are not preserved in the final
-- terminal input state after the Brick application returns the terminal
-- to the user.
suspendAndResume :: (Ord n) => IO s -> EventM n s ()
suspendAndResume act = suspendAndResume' act >>= put

-- | Suspend the event loop, save the terminal state, and run the
-- specified action. When it completes, restore the terminal state,
-- empty the rendering cache, return the result, and continue execution
-- of the event handler that called this.
--
-- Note that any changes made to the terminal's input state are ignored
-- when Brick resumes execution and are not preserved in the final
-- terminal input state after the Brick application returns the terminal
-- to the user.
suspendAndResume' :: (Ord n) => IO a -> EventM n s a
suspendAndResume' act = do
    ctx <- getVtyContext
    liftIO $ shutdownVtyContext ctx
    result <- liftIO act
    setVtyContext =<< (liftIO $ newVtyContextFrom ctx)
    invalidateCache
    return result

-- | Request that the specified UI element be made visible on the
-- next rendering. This is provided to allow event handlers to make
-- visibility requests in the same way that the 'visible' function does
-- at rendering time.
makeVisible :: (Ord n) => n -> EventM n s ()
makeVisible n = EventM $ do
    lift $ lift $ modify (\s -> s { requestedVisibleNames = S.insert n $ requestedVisibleNames s })
