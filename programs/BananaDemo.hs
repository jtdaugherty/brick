{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where



import Brick.MainBanana
import Brick.Types

import qualified Reactive.Banana as Banana
import qualified Reactive.Banana.Frameworks as Banana

import Graphics.Vty.Input.Events

import Control.Concurrent.MVar

import Lens.Micro ((<&>))

import Brick.Widgets.Core

import Data.Default



main :: IO ()
main = do

  finMVar <- newEmptyMVar
  startup@(_, startupH) <- Banana.newAddHandler

  network <- Banana.compile $ brickNetwork startup $ \eventE finE -> do

    Banana.reactimate $ finE <&> \() -> putMVar finMVar ()

    curPromptStr <- Banana.accumB "" $ eventE <&> \case
      Just (EvKey key _mods) -> case key of
        KEsc    -> const ""
        KEnter  -> const ""
        KChar c -> (++[c])
        KBS     -> init
        _       -> id
      _                      -> id


    let promptWidget :: Banana.Behavior (Widget String) =
          curPromptStr <&> \s -> str $ if null s then " " else s
    let lengthWidget :: Banana.Behavior (Widget String) =
          curPromptStr <&> str . show . length

    let nextE = eventE <&> \case
          Just (EvKey KEsc _) -> halt
          _                   -> continue

    let widgetsB =
          (\wid1 wid2 -> [wid1 <=> wid2 <=> emptyWidget])
            <$> promptWidget
            <*> lengthWidget

    let cursorB = pure $ const Nothing
    let attrB   = pure $ def

    return $ (nextE, widgetsB, cursorB, attrB)


  Banana.actuate network
  startupH ()
  takeMVar finMVar
  Banana.pause network
