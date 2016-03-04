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

  -- * Cursor management functions
  , neverShowCursor
  , showFirstCursor
  , showCursorNamed
  )
where

import Control.Exception (finally)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State
import Control.Monad.Trans.Reader
import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan, killThread)
import Data.Default
import Data.Maybe (listToMaybe)
import qualified Data.Map as M
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
  )

import Brick.Types (Viewport, Direction, Widget, rowL, columnL, CursorLocation(..), cursorLocationNameL, EventM(..))
import Brick.Types.Internal (ScrollRequest(..), RenderState(..), Next(..))
import Brick.Widgets.Internal (renderFinal)
import Brick.AttrMap

-- | The library application abstraction. Your application's operations
-- are represented here and passed to one of the various main functions
-- in this module. An application is in terms of an application state
-- type 's' and an application event type 'e'. In the simplest case 'e' is
-- vty's 'Event' type, but you may define your own event type, permitted
-- that it has a constructor for wrapping Vty events, so that Vty events
-- can be handled by your event loop.
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
        , appHandleEvent :: s -> e -> EventM n (Next s)
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
        , appLiftVtyEvent :: Event -> e
        -- ^ The event constructor to use to wrap Vty events in your own
        -- event type. For example, if the application's event type is
        -- 'Event', this is just 'id'.
        }

-- | The default main entry point which takes an application and an
-- initial state and returns the final state returned by a 'halt'
-- operation.
defaultMain :: App s Event n
            -- ^ The application.
            -> s
            -- ^ The initial application state.
            -> IO s
defaultMain app st = do
    chan <- newChan
    customMain (mkVty def) chan app st

-- | A simple main entry point which takes a widget and renders it. This
-- event loop terminates when the user presses any key, but terminal
-- resize events cause redraws.
simpleMain :: Widget n
           -- ^ The widget to draw.
           -> IO ()
simpleMain w =
    let app = App { appDraw = const [w]
                  , appHandleEvent = resizeOrQuit
                  , appStartEvent = return
                  , appAttrMap = def
                  , appLiftVtyEvent = id
                  , appChooseCursor = neverShowCursor
                  }
    in defaultMain app ()

-- | An event-handling function which continues execution of the event
-- loop only when resize events occur; all other types of events trigger
-- a halt. This is a convenience function useful as an 'appHandleEvent'
-- value for simple applications using the 'Event' type that do not need
-- to get more sophisticated user input.
resizeOrQuit :: s -> Event -> EventM n (Next s)
resizeOrQuit s (EvResize _ _) = continue s
resizeOrQuit s _ = halt s

data InternalNext n a = InternalSuspendAndResume (RenderState n) (IO a)
                      | InternalHalt a

runWithNewVty :: IO Vty -> Chan e -> App s e n -> RenderState n -> s -> IO (InternalNext n s)
runWithNewVty buildVty chan app initialRS initialSt =
    withVty buildVty $ \vty -> do
        pid <- forkIO $ supplyVtyEvents vty (appLiftVtyEvent app) chan
        let runInner rs st = do
              (result, newRS) <- runVty vty chan app st rs
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
customMain :: IO Vty
           -- ^ An IO action to build a Vty handle. This is used to
           -- build a Vty handle whenever the event loop begins or is
           -- resumed after suspension.
           -> Chan e
           -- ^ An event channel for sending custom events to the event
           -- loop (you write to this channel, the event loop reads from
           -- it).
           -> App s e n
           -- ^ The application.
           -> s
           -- ^ The initial application state.
           -> IO s
customMain buildVty chan app initialAppState = do
    let run rs st = do
            result <- runWithNewVty buildVty chan app rs st
            case result of
                InternalHalt s -> return s
                InternalSuspendAndResume newRS action -> do
                    newAppState <- action
                    run newRS newAppState

    (st, initialScrollReqs) <- runStateT (runReaderT (runEventM (appStartEvent app initialAppState)) M.empty) []
    let initialRS = RS M.empty initialScrollReqs
    run initialRS st

supplyVtyEvents :: Vty -> (Event -> e) -> Chan e -> IO ()
supplyVtyEvents vty mkEvent chan =
    forever $ do
        e <- nextEvent vty
        writeChan chan $ mkEvent e

runVty :: Vty -> Chan e -> App s e n -> s -> RenderState n -> IO (Next s, RenderState n)
runVty vty chan app appState rs = do
    firstRS <- renderApp vty app appState rs
    e <- readChan chan
    (next, scrollReqs) <- runStateT (runReaderT (runEventM (appHandleEvent app appState e)) (viewportMap rs)) []
    return (next, firstRS { scrollRequests = scrollReqs })

-- | Given a viewport name, get the viewport's size and offset
-- information from the most recent rendering. Returns 'Nothing' if
-- no such state could be found, either because the name was invalid
-- or because no rendering has occurred (e.g. in an 'appStartEvent'
-- handler).
lookupViewport :: (Ord n) => n -> EventM n (Maybe Viewport)
lookupViewport = EventM . asks . M.lookup

withVty :: IO Vty -> (Vty -> IO a) -> IO a
withVty buildVty useVty = do
    vty <- buildVty
    useVty vty `finally` shutdown vty

renderApp :: Vty -> App s e n -> s -> RenderState n -> IO (RenderState n)
renderApp vty app appState rs = do
    sz <- displayBounds $ outputIface vty
    let (newRS, pic, theCursor) = renderFinal (appAttrMap app appState)
                                    (appDraw app appState)
                                    sz
                                    (appChooseCursor app appState)
                                    rs
        picWithCursor = case theCursor of
            Nothing -> pic { picCursor = NoCursor }
            Just loc -> pic { picCursor = Cursor (loc^.columnL) (loc^.rowL) }

    update vty picWithCursor

    return newRS

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

-- | Show the cursor with the specified name, if such a cursor location
-- has been reported.
showCursorNamed :: (Eq n) => n -> [CursorLocation n] -> Maybe (CursorLocation n)
showCursorNamed name locs =
    let matches loc = loc^.cursorLocationNameL == Just name
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
                   }

-- | Build a viewport scroller for the viewport with the specified name.
viewportScroll :: n -> ViewportScroll n
viewportScroll n =
    ViewportScroll { viewportName = n
                   , hScrollPage = \dir -> EventM $ lift $ modify ((n, HScrollPage dir) :)
                   , hScrollBy = \i -> EventM $ lift $ modify ((n, HScrollBy i) :)
                   , hScrollToBeginning = EventM $ lift $ modify ((n, HScrollToBeginning) :)
                   , hScrollToEnd = EventM $ lift $ modify ((n, HScrollToEnd) :)
                   , vScrollPage = \dir -> EventM $ lift $ modify ((n, VScrollPage dir) :)
                   , vScrollBy = \i -> EventM $ lift $ modify ((n, VScrollBy i) :)
                   , vScrollToBeginning = EventM $ lift $ modify ((n, VScrollToBeginning) :)
                   , vScrollToEnd = EventM $ lift $ modify ((n, VScrollToEnd) :)
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
