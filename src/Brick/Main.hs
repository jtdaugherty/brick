module Brick.Main
  ( App(..)
  , defaultMain
  , customMain
  , simpleMain
  , resizeOrQuit

  -- * Event handler functions
  , EventM
  , Next
  , continue
  , halt
  , suspendAndResume

  -- ** Viewport scrolling
  , viewportScroll
  , ViewportScroll
  , scrollBy
  , scrollPage
  , scrollToBeginning
  , scrollToEnd

  -- * Cursor management functions
  , neverShowCursor
  , showFirstCursor
  )
where

import Control.Exception (finally)
import Control.Lens ((^.))
import Control.Monad (forever)
import Control.Monad.Trans.State
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

import Brick.Widgets.Core (Widget)
import Brick.Widgets.Internal (renderFinal, RenderState(..), ScrollRequest(..), Direction(..))
import Brick.Core (row, column, CursorLocation(..), Name(..))
import Brick.AttrMap

-- | The type of actions to take in an event handler.
data Next a = Continue a
            | SuspendAndResume (IO a)
            | Halt a

-- | The library application abstraction. Your application's operations
-- are represented here and passed to one of the various main functions
-- in this module. An application is in terms of an application state
-- type 's' and an application event type 'e'. In the simplest case 'e' is
-- vty's 'Event' type, but you may define your own event type, permitted
-- that it has a constructor for wrapping Vty events, so that Vty events
-- can be handled by your event loop.
data App s e =
    App { appDraw :: s -> [Widget]
        -- ^ This function turns your application state into a list of
        -- widget layers. The layers are listed topmost first.
        , appChooseCursor :: s -> [CursorLocation] -> Maybe CursorLocation
        -- ^ This function chooses which of the zero or more cursor
        -- locations reported by the rendering process should be
        -- selected as the one to use to place the cursor. If this
        -- returns 'Nothing', no cursor is placed. The rationale here
        -- is that many widgets may request a cursor placement but your
        -- application state is what you probably want to use to decide
        -- which one wins.
        , appHandleEvent :: s -> e -> EventM (Next s)
        -- ^ This function takes an event and your application state
        -- and returns an action to be taken. Possible options are
        -- 'continue', 'suspendAndResume', and 'halt'.
        , appStartEvent :: s -> EventM s
        -- ^ This function gets called once just prior to the first
        -- drawing of your application. Here is where you can make
        -- initial scrolling requests, for example.
        , appAttrMap :: s -> AttrMap
        -- ^ The attribute map that should be used during rendering.
        , appMakeVtyEvent :: Event -> e
        -- ^ The event constructor to use to wrap Vty events in your own
        -- event type. For example, if the application's event type is
        -- 'Event', this is just 'id'.
        }

-- | The monad in which event handlers run.
type EventM a = StateT EventState IO a

type EventState = [(Name, ScrollRequest)]

-- | The default main entry point which takes an application and an
-- initial state and returns the final state returned by a 'halt'
-- operation.
defaultMain :: App s Event
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
simpleMain :: Widget
           -- ^ The widget to draw.
           -> IO ()
simpleMain w =
    let app = App { appDraw = const [w]
                  , appHandleEvent = resizeOrQuit
                  , appStartEvent = return
                  , appAttrMap = def
                  , appMakeVtyEvent = id
                  , appChooseCursor = neverShowCursor
                  }
    in defaultMain app ()

-- | An event-handling function which continues execution of the event
-- loop only when resize events occur; all other types of events trigger
-- a halt. This is a convenience function useful as an 'appHandleEvent'
-- value for simple applications using the 'Event' type that do not need
-- to get more sophisticated user input.
resizeOrQuit :: s -> Event -> EventM (Next s)
resizeOrQuit a e =
    case e of
        EvResize _ _ -> continue a
        _ -> halt a

data InternalNext a = InternalSuspendAndResume RenderState (IO a)
                    | InternalHalt a

runWithNewVty :: IO Vty -> Chan e -> App s e -> RenderState -> s -> IO (InternalNext s)
runWithNewVty buildVty chan app initialRS initialSt = do
    withVty buildVty $ \vty -> do
        pid <- forkIO $ supplyVtyEvents vty (appMakeVtyEvent app) chan
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
           -> App s e
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

    (st, initialScrollReqs) <- runStateT (appStartEvent app initialAppState) []
    let initialRS = RS M.empty initialScrollReqs
    run initialRS st

supplyVtyEvents :: Vty -> (Event -> e) -> Chan e -> IO ()
supplyVtyEvents vty mkEvent chan =
    forever $ do
        e <- nextEvent vty
        writeChan chan $ mkEvent e

runVty :: Vty -> Chan e -> App s e -> s -> RenderState -> IO (Next s, RenderState)
runVty vty chan app appState rs = do
    firstRS <- renderApp vty app appState rs
    e <- readChan chan
    (next, scrollReqs) <- runStateT (appHandleEvent app appState e) []
    return (next, firstRS { _scrollRequests = scrollReqs })

withVty :: IO Vty -> (Vty -> IO a) -> IO a
withVty buildVty useVty = do
    vty <- buildVty
    useVty vty `finally` shutdown vty

renderApp :: Vty -> App s e -> s -> RenderState -> IO RenderState
renderApp vty app appState rs = do
    sz <- displayBounds $ outputIface vty
    let (newRS, pic, theCursor) = renderFinal (appAttrMap app appState)
                                    (appDraw app appState)
                                    sz
                                    (appChooseCursor app appState)
                                    rs
        picWithCursor = case theCursor of
            Nothing -> pic { picCursor = NoCursor }
            Just loc -> pic { picCursor = Cursor (loc^.column) (loc^.row) }

    update vty picWithCursor

    return newRS

-- | Ignore all requested cursor positions returned by the rendering
-- process. This is a convenience function useful as an
-- 'appChooseCursor' value when a simple application has no need to
-- position the cursor.
neverShowCursor :: s -> [CursorLocation] -> Maybe CursorLocation
neverShowCursor = const $ const Nothing

-- | Always show the first cursor, if any, returned by the rendering
-- process. This is a convenience function useful as an
-- 'appChooseCursor' value when a simple program has zero or more
-- widgets that advertise a cursor position.
showFirstCursor :: s -> [CursorLocation] -> Maybe CursorLocation
showFirstCursor = const $ listToMaybe

-- | A viewport scrolling handle for managing the scroll state of
-- viewports.
data ViewportScroll =
    ViewportScroll { viewportName :: Name
                   , scrollPage :: Direction -> EventM ()
                   , scrollBy :: Int -> EventM ()
                   , scrollToBeginning :: EventM ()
                   , scrollToEnd :: EventM ()
                   }

-- | Build a viewport scroller for the viewport with the specified name.
viewportScroll :: Name -> ViewportScroll
viewportScroll n =
    ViewportScroll { viewportName = n
                   , scrollPage = \dir -> modify ((n, ScrollPage dir) :)
                   , scrollBy = \i -> modify ((n, ScrollBy i) :)
                   , scrollToBeginning = modify ((n, ScrollToBeginning) :)
                   , scrollToEnd = modify ((n, ScrollToEnd) :)
                   }

-- | Continue running the event loop with the specified application
-- state.
continue :: s -> EventM (Next s)
continue = return . Continue

-- | Halt the event loop and return the specified application state as
-- the final state value.
halt :: s -> EventM (Next s)
halt = return . Halt

-- | Suspend the event loop, save the terminal state, and run the
-- specified action. When it returns an application state value, restore
-- the terminal state, and resume the event loop with the returned
-- application state.
suspendAndResume :: IO s -> EventM (Next s)
suspendAndResume = return . SuspendAndResume
