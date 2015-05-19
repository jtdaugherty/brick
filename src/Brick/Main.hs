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
import System.Exit (exitSuccess)

import Brick.Prim (Prim)
import Brick.Prim.Internal (renderFinal)
import Brick.Core (Location(..), CursorLocation(..))

data App a e =
    App { appDraw :: a -> [Prim a]
        , appChooseCursor :: a -> [CursorLocation] -> Maybe CursorLocation
        , appHandleEvent :: e -> a -> IO a
        }

instance Default (App a e) where
    def = App { appDraw = const def
              , appChooseCursor = neverShowCursor
              , appHandleEvent = const return
              }

defaultMain :: App a Event -> a -> IO ()
defaultMain = defaultMainWithVty (mkVty def)

simpleMain :: [Prim ()] -> IO ()
simpleMain ls =
    let app = def { appDraw = const ls
                  , appHandleEvent = const $ const exitSuccess
                  }
    in defaultMain app ()

defaultMainWithVty :: IO Vty -> App a Event -> a -> IO ()
defaultMainWithVty buildVty app initialState = do
    chan <- newChan
    withVty buildVty $ \vty -> do
        forkIO $ supplyVtyEvents vty id chan
        runVty vty chan app initialState

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

runVty :: Vty -> Chan e -> App a e -> a -> IO ()
runVty vty chan app appState = do
    state' <- renderApp vty app appState
    e <- readChan chan
    appHandleEvent app e state' >>= runVty vty chan app

withVty :: IO Vty -> (Vty -> IO a) -> IO a
withVty buildVty useVty = do
    vty <- buildVty
    useVty vty `finally` shutdown vty

renderApp :: Vty -> App a e -> a -> IO a
renderApp vty app appState = do
    sz <- displayBounds $ outputIface vty
    let (newAppState, pic, theCursor) = renderFinal (appDraw app appState) sz (appChooseCursor app appState) appState
        picWithCursor = case theCursor of
            Nothing -> pic { picCursor = NoCursor }
            Just (CursorLocation (Location (w, h)) _) -> pic { picCursor = Cursor w h }

    update vty picWithCursor

    return newAppState

neverShowCursor :: a -> [CursorLocation] -> Maybe CursorLocation
neverShowCursor = const $ const Nothing

showFirstCursor :: a -> [CursorLocation] -> Maybe CursorLocation
showFirstCursor = const $ listToMaybe
