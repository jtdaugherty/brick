module Brick.Main
  ( App(..)
  , defaultMain
  , defaultMainWithVty

  , EventM
  , Next
  , continue
  , halt
  , suspendAndResume

  , viewportScroll
  , ViewportScroll(scrollBy, scrollPage, scrollToBeginning, scrollToEnd)

  , simpleMain

  , supplyVtyEvents

  , neverShowCursor
  , showFirstCursor
  )
where

import Control.Exception (finally)
import Control.Monad (forever)
import Control.Monad.Trans.State
import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan, killThread)
import Data.Default
import Data.Monoid
import Data.Maybe (listToMaybe)
import qualified Data.Map as M
import Graphics.Vty
  ( Vty
  , Picture(..)
  , Cursor(..)
  , Event(..)
  , Attr
  , update
  , outputIface
  , displayBounds
  , shutdown
  , nextEvent
  , mkVty
  )

import Brick.Widgets.Core (Widget)
import Brick.Widgets.Internal (renderFinal, RenderState(..), ScrollRequest(..), Direction(..))
import Brick.Core (Location(..), CursorLocation(..), Name(..))
import Brick.AttrMap

data Next a = Continue a
            | SuspendAndResume (IO a)
            | Halt a

data App a e =
    App { appDraw :: a -> [Widget]
        , appChooseCursor :: a -> [CursorLocation] -> Maybe CursorLocation
        , appHandleEvent :: e -> a -> EventM (Next a)
        , appAttrMap :: a -> AttrMap
        }

instance Default (App a e) where
    def = App { appDraw = const def
              , appChooseCursor = neverShowCursor
              , appHandleEvent = const (return . Continue)
              , appAttrMap = const def
              }

type EventM a = StateT EventState IO a

type EventState = [(Name, ScrollRequest)]

defaultMain :: App a Event -> a -> IO a
defaultMain = defaultMainWithVty (mkVty def)

simpleMain :: [(AttrName, Attr)] -> [Widget] -> IO ()
simpleMain attrs ls =
    let app = def { appDraw = const ls
                  , appHandleEvent = const (return . Halt)
                  , appAttrMap = const $ attrMap def attrs
                  }
    in defaultMain app ()

data InternalNext a = InternalSuspendAndResume RenderState (IO a)
                    | InternalHalt a

runWithNewVty :: IO Vty -> App a Event -> RenderState -> a -> IO (InternalNext a)
runWithNewVty buildVty app initialRS initialSt = do
    chan <- newChan
    withVty buildVty $ \vty -> do
        pid <- forkIO $ supplyVtyEvents vty id chan
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

defaultMainWithVty :: IO Vty -> App a Event -> a -> IO a
defaultMainWithVty buildVty app initialAppState = do
    let initialRS = RS M.empty mempty
        run rs st = do
            result <- runWithNewVty buildVty app rs st
            case result of
                InternalHalt s -> return s
                InternalSuspendAndResume newRS action -> do
                    newAppState <- action
                    run newRS newAppState

    run initialRS initialAppState

supplyVtyEvents :: Vty -> (Event -> e) -> Chan e -> IO ()
supplyVtyEvents vty mkEvent chan =
    forever $ do
        e <- nextEvent vty
        writeChan chan $ mkEvent e

runVty :: Vty -> Chan e -> App a e -> a -> RenderState -> IO (Next a, RenderState)
runVty vty chan app appState rs = do
    firstRS <- renderApp vty app appState rs
    e <- readChan chan
    (next, scrollReqs) <- runStateT (appHandleEvent app e appState) []
    return (next, firstRS { _scrollRequests = scrollReqs })

withVty :: IO Vty -> (Vty -> IO a) -> IO a
withVty buildVty useVty = do
    vty <- buildVty
    useVty vty `finally` shutdown vty

renderApp :: Vty -> App a e -> a -> RenderState -> IO RenderState
renderApp vty app appState rs = do
    sz <- displayBounds $ outputIface vty
    let (newRS, pic, theCursor) = renderFinal (appAttrMap app appState)
                                    (appDraw app appState)
                                    sz
                                    (appChooseCursor app appState)
                                    rs
        picWithCursor = case theCursor of
            Nothing -> pic { picCursor = NoCursor }
            Just (CursorLocation (Location (w, h)) _) -> pic { picCursor = Cursor w h }

    update vty picWithCursor

    return newRS

neverShowCursor :: a -> [CursorLocation] -> Maybe CursorLocation
neverShowCursor = const $ const Nothing

showFirstCursor :: a -> [CursorLocation] -> Maybe CursorLocation
showFirstCursor = const $ listToMaybe

data ViewportScroll =
    ViewportScroll { viewportName :: Name
                   , scrollPage :: Direction -> EventM ()
                   , scrollBy :: Int -> EventM ()
                   , scrollToBeginning :: EventM ()
                   , scrollToEnd :: EventM ()
                   }

viewportScroll :: Name -> ViewportScroll
viewportScroll n =
    ViewportScroll { viewportName = n
                   , scrollPage = \dir -> modify ((n, ScrollPage dir) :)
                   , scrollBy = \i -> modify ((n, ScrollBy i) :)
                   , scrollToBeginning = modify ((n, ScrollToBeginning) :)
                   , scrollToEnd = modify ((n, ScrollToEnd) :)
                   }

continue :: a -> EventM (Next a)
continue = return . Continue

halt :: a -> EventM (Next a)
halt = return . Halt

suspendAndResume :: IO a -> EventM (Next a)
suspendAndResume = return . SuspendAndResume
