{-# LANGUAGE ScopedTypeVariables #-}
module Brick.Main
  ( App(..)
  , defaultMain
  , customMain
  , simpleMain
  , resizeOrQuit

  -- * Event handler functions
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
import Control.Monad.State
import Control.Monad.Reader
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
import Brick.Types
  ( Widget
  , EventM(..)
  , EventNext(..)
  , liftRawEventHandler
  , EventHandler(..))
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
        , appHandleEvent :: BrickEvent n e -> EventM n s ()
        -- ^ This function takes the current application state and an
        -- event and returns an action to be taken and a corresponding
        -- transformed application state. Possible options are
        -- 'continue', 'suspendAndResume', and 'halt'.
        , appStartEvent :: EventM n s ()
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
                  , appStartEvent = pure ()
                  , appAttrMap = const $ attrMap defAttr []
                  , appChooseCursor = neverShowCursor
                  }
    in defaultMain app ()

-- | An event-handling function which continues execution of the event
-- loop only when resize events occur; all other types of events trigger
-- a halt. This is a convenience function useful as an 'appHandleEvent'
-- value for simple applications using the 'Event' type that do not need
-- to get more sophisticated user input.
resizeOrQuit :: BrickEvent n e -> EventM n s ()
resizeOrQuit (VtyEvent (EvResize _ _)) = pure ()
resizeOrQuit _ = halt

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
    brickChan <- newBChan 20
    let eventRO = EventRO M.empty initialVty mempty
    let run vty rs st = do
            (newState, internalNext) <- runWithNewVty vty brickChan mUserChan app rs st
            case internalNext of
                InternalHalt -> pure newState
                InternalSuspendAndResume newRS eState action -> do
                    cont <- action
                    newVty <- buildVty
                    loop newVty cont newState eState newRS


        loop vty eventm st es rs = do
          (st', es', innerNext) <- runEventMLoop eventRO eventm st es
          case innerNext of
            InnerInternalHalt -> pure st'
            InnerInternalSuspendAndResume act -> do
                em <- act
                loop vty em st' es' rs
            InnerInternalDone () -> do
                run vty rs st'
    let emptyES = ES [] []
    let initialRS = RS M.empty (esScrollRequests emptyES) S.empty mempty []
    loop initialVty (appStartEvent app) initialAppState emptyES initialRS

data InternalNext n s a = InternalSuspendAndResume (RenderState n) (EventState n) (IO (EventM n s a))
                        | InternalHalt

readBrickEvent :: BChan (BrickEvent n e) -> BChan e -> IO (BrickEvent n e)
readBrickEvent brickChan userChan = either id AppEvent <$> readBChan2 brickChan userChan

runWithNewVty :: (Ord n)
              => Vty
              -> BChan (BrickEvent n e)
              -> Maybe (BChan e)
              -> App s e n
              -> RenderState n
              -> s
              -> IO (s, InternalNext n s ())
runWithNewVty buildVty brickChan mUserChan app initialRS initialSt =
    withVty buildVty $ \vty -> do
        pid <- forkIO $ supplyVtyEvents vty brickChan
        let readEvent = case mUserChan of
              Nothing -> readBChan brickChan
              Just uc -> readBrickEvent brickChan uc
            runInner rs st = do
              (newState, internalNext, newRS, eState) <- runVty vty readEvent app st (rs & observedNamesL .~ S.empty
                                                                 & clickableNamesL .~ mempty)
              case internalNext of
                  InnerInternalSuspendAndResume act -> do
                      killThread pid
                      return (newState, InternalSuspendAndResume newRS eState act)
                  InnerInternalDone () -> runInner newRS newState
                  InnerInternalHalt -> do
                      killThread pid
                      return (newState, InternalHalt)
        runInner initialRS initialSt

supplyVtyEvents :: Vty -> BChan (BrickEvent n e) -> IO ()
supplyVtyEvents vty chan =
    forever $ do
        e <- nextEvent vty
        writeBChan chan $ VtyEvent e

data InnerInternalNext n s a
  = InnerInternalSuspendAndResume (IO (EventM n s a))
  | InnerInternalDone a
  | InnerInternalHalt

runVty :: (Ord n)
       => Vty
       -> IO (BrickEvent n e)
       -> App s e n
       -> s
       -> RenderState n
       -> IO (s, InnerInternalNext n s (), RenderState n, EventState n)
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

    let eventRO = EventRO (viewportMap nextRS) vty nextExts

    let emptyES = ES [] []

    (newAppState, eState, innerNext) <- runEventMLoop eventRO (appHandleEvent app e') appState emptyES
    let newRS = nextRS { rsScrollRequests = esScrollRequests eState
                       , renderCache = applyInvalidations (cacheInvalidateRequests eState) $
                                       renderCache nextRS
                       } 
    pure (newAppState, innerNext, newRS, eState)

runEventMLoop
  :: forall n s a.
     EventRO n
  -> EventM n s a
  -> s
  -> EventState n
  -> IO (s, EventState n, InnerInternalNext n s a)
runEventMLoop ro em_ st_ es_ = go em_ st_ es_
  where
    go :: EventM n s a -> s -> EventState n -> IO (s, EventState n, InnerInternalNext n s a)
    go em st es = do
      let (st', enext) = runEventM em st :: (s, EventNext n s a)
      case enext of
        Halt -> pure (st', es, InnerInternalHalt)
        Pure a -> pure (st', es, InnerInternalDone a)
        SuspendAndResume ioa -> pure (st', es, InnerInternalSuspendAndResume ioa)
        Resume eh -> do
          (nextEm, es') <- runStateT (runReaderT (runEventHandler eh) ro) es
          go nextEm st es'

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
lookupViewport :: (Ord n) => n -> EventM n s (Maybe Viewport)
lookupViewport n = liftRawEventHandler $ asks (M.lookup n . eventViewportMap)

-- | Did the specified mouse coordinates (column, row) intersect the
-- specified extent?
clickedExtent :: (Int, Int) -> Extent n -> Bool
clickedExtent (c, r) (Extent _ (Location (lc, lr)) (w, h) _) =
   c >= lc && c < (lc + w) &&
   r >= lr && r < (lr + h)

-- | Given a resource name, get the most recent rendering extent for the
-- name (if any).
lookupExtent :: (Eq n) => n -> EventM n s (Maybe (Extent n))
lookupExtent n = liftRawEventHandler $ asks (listToMaybe . filter f . latestExtents)
    where
        f (Extent n' _ _ _) = n == n'

-- | Given a mouse click location, return the extents intersected by the
-- click. The returned extents are sorted such that the first extent in
-- the list is the most specific extent and the last extent is the most
-- generic (top-level). So if two extents A and B both intersected the
-- mouse click but A contains B, then they would be returned [B, A].
findClickedExtents :: (Int, Int) -> EventM n s [Extent n]
findClickedExtents pos = liftRawEventHandler $ asks (findClickedExtents_ pos . latestExtents)

findClickedExtents_ :: (Int, Int) -> [Extent n] -> [Extent n]
findClickedExtents_ pos = reverse . filter (clickedExtent pos)

-- | Get the Vty handle currently in use.
getVtyHandle :: EventM n s Vty
getVtyHandle = liftRawEventHandler $ asks eventVtyHandle

-- | Invalidate the rendering cache entry with the specified resource
-- name.
invalidateCacheEntry :: n -> EventM n s ()
invalidateCacheEntry n = liftRawEventHandler $
    modify (\s -> s { cacheInvalidateRequests = InvalidateSingle n : cacheInvalidateRequests s })

-- | Invalidate the entire rendering cache.
invalidateCache :: EventM n s ()
invalidateCache = liftRawEventHandler $
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
data ViewportScroll n s =
    ViewportScroll { viewportName :: n
                   -- ^ The name of the viewport to be controlled by
                   -- this scrolling handle.
                   , hScrollPage :: Direction -> EventM n s ()
                   -- ^ Scroll the viewport horizontally by one page in
                   -- the specified direction.
                   , hScrollBy :: Int -> EventM n s ()
                   -- ^ Scroll the viewport horizontally by the
                   -- specified number of rows or columns depending on
                   -- the orientation of the viewport.
                   , hScrollToBeginning :: EventM n s ()
                   -- ^ Scroll horizontally to the beginning of the
                   -- viewport.
                   , hScrollToEnd :: EventM n s ()
                   -- ^ Scroll horizontally to the end of the viewport.
                   , vScrollPage :: Direction -> EventM n s ()
                   -- ^ Scroll the viewport vertically by one page in
                   -- the specified direction.
                   , vScrollBy :: Int -> EventM n s ()
                   -- ^ Scroll the viewport vertically by the specified
                   -- number of rows or columns depending on the
                   -- orientation of the viewport.
                   , vScrollToBeginning :: EventM n s ()
                   -- ^ Scroll vertically to the beginning of the viewport.
                   , vScrollToEnd :: EventM n s ()
                   -- ^ Scroll vertically to the end of the viewport.
                   , setTop :: Int -> EventM n s ()
                   -- ^ Set the top row offset of the viewport.
                   , setLeft :: Int -> EventM n s ()
                   -- ^ Set the left column offset of the viewport.
                   }

addScrollRequest :: (n, ScrollRequest) -> EventM n s ()
addScrollRequest req = liftRawEventHandler $
    modify (\s -> s { esScrollRequests = req : esScrollRequests s })

-- | Build a viewport scroller for the viewport with the specified name.
viewportScroll :: n -> ViewportScroll n s
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

-- | Halt the event loop and return the specified application state as
-- the final state value.
halt :: EventM n s ()
halt = EventM $ \s -> (s, Halt)

-- | Suspend the event loop, save the terminal state, and run the
-- specified action. When it returns an application state value, restore
-- the terminal state, redraw the application from the new state, and
-- resume the event loop.
suspendAndResume :: IO a -> EventM n s a
suspendAndResume func = EventM $ \s -> (s, SuspendAndResume $ do
  a <- func
  pure $ pure a)
