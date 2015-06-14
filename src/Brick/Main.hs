{-# LANGUAGE BangPatterns #-}
module Brick.Main
  ( App(..)
  , defaultMain
  , defaultMainWithVty

  , simpleMain

  , supplyVtyEvents
  , withVty
  , runVty

  , neverShowCursor
  , showFirstCursor
  )
where

import Control.Exception (finally)
import Control.Monad (when, forever)
import Control.Concurrent (forkIO, Chan, newChan, readChan, writeChan)
import Data.Default
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
import System.Exit (exitSuccess)

import Brick.Render (Render)
import Brick.Render.Internal (renderFinal, RenderState(..))
import Brick.Core (Location(..), CursorLocation(..))
import Brick.AttrMap

data App a e =
    App { appDraw :: a -> [Render]
        , appChooseCursor :: a -> [CursorLocation] -> Maybe CursorLocation
        , appHandleEvent :: e -> a -> IO a
        , appAttrMap :: a -> AttrMap
        }

instance Default (App a e) where
    def = App { appDraw = const def
              , appChooseCursor = neverShowCursor
              , appHandleEvent = const return
              , appAttrMap = const def
              }

defaultMain :: App a Event -> a -> IO ()
defaultMain = defaultMainWithVty (mkVty def)

simpleMain :: [(AttrName, Attr)] -> [Render] -> IO ()
simpleMain attrs ls =
    let app = def { appDraw = const ls
                  , appHandleEvent = const $ const exitSuccess
                  , appAttrMap = const $ attrMap def attrs
                  }
    in defaultMain app ()

defaultMainWithVty :: IO Vty -> App a Event -> a -> IO ()
defaultMainWithVty buildVty app initialAppState = do
    let initialRS = RS M.empty
    chan <- newChan
    withVty buildVty $ \vty -> do
        forkIO $ supplyVtyEvents vty id chan
        runVty vty chan app initialAppState initialRS

isResizeEvent :: Event -> Bool
isResizeEvent (EvResize _ _) = True
isResizeEvent _ = False

supplyVtyEvents :: Vty -> (Event -> e) -> Chan e -> IO ()
supplyVtyEvents vty mkEvent chan =
    forever $ do
        e <- nextEvent vty
        -- On resize, send two events to force two redraws to force all
        -- state updates to get flushed to the display
        when (isResizeEvent e) $ writeChan chan $ mkEvent e
        writeChan chan $ mkEvent e

runVty :: Vty -> Chan e -> App a e -> a -> RenderState -> IO ()
runVty vty chan app appState rs = do
    newRS <- renderApp vty app appState rs
    e <- readChan chan
    newAppState <- appHandleEvent app e appState
    runVty vty chan app newAppState newRS

withVty :: IO Vty -> (Vty -> IO a) -> IO a
withVty buildVty useVty = do
    vty <- buildVty
    useVty vty `finally` shutdown vty

renderApp :: Vty -> App a e -> a -> RenderState -> IO RenderState
renderApp vty app appState rs = do
    sz <- displayBounds $ outputIface vty
    let (newRS, pic, theCursor) = renderFinal (appAttrMap app appState) (appDraw app appState) sz (appChooseCursor app appState) rs
        picWithCursor = case theCursor of
            Nothing -> pic { picCursor = NoCursor }
            Just (CursorLocation (Location (w, h)) _) -> pic { picCursor = Cursor w h }

    update vty picWithCursor

    return newRS

neverShowCursor :: a -> [CursorLocation] -> Maybe CursorLocation
neverShowCursor = const $ const Nothing

showFirstCursor :: a -> [CursorLocation] -> Maybe CursorLocation
showFirstCursor = const $ listToMaybe
