{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad (void, forever)
import Control.Concurrent (newChan, writeChan, threadDelay, forkIO)
import Data.Default
import Data.Monoid
import Graphics.Vty hiding (translate)

import Brick.Main
import Brick.Widgets.Core

data St =
    St { _stLastVtyEvent :: Maybe Event
       , _stCounter :: Int
       }

makeLenses ''St

data CustomEvent = VtyEvent Event
                 | Counter

drawUI :: St -> [Widget]
drawUI st = [a]
    where
        a = (str $ "Last Vty event: " <> (show $ st^.stLastVtyEvent))
            <=>
            (str $ "Counter value is: " <> (show $ st^.stCounter))

appEvent :: CustomEvent -> St -> EventM (Next St)
appEvent e st =
    case e of
        VtyEvent (EvKey KEsc []) -> halt st
        VtyEvent ev -> continue $ st & stLastVtyEvent .~ (Just ev)
        Counter -> continue $ st & stCounter %~ (+1)

initialState :: St
initialState =
    St { _stLastVtyEvent = Nothing
       , _stCounter = 0
       }

theApp :: App St CustomEvent
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appAttrMap = def
        , appMakeVtyEvent = VtyEvent
        }

main :: IO ()
main = do
    chan <- newChan

    forkIO $ forever $ do
        writeChan chan Counter
        threadDelay 1000000

    void $ customMain (mkVty def) chan theApp initialState
