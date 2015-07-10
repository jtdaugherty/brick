{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (^.), (&), (.~), (%~))
import Control.Monad (void, forever)
import Control.Concurrent (newChan, writeChan, threadDelay, forkIO)
import Data.Default
import Data.Monoid
import qualified Graphics.Vty as V

import Brick.Main
  ( App(..)
  , Next
  , EventM
  , showFirstCursor
  , customMain
  , continue
  , halt
  )
import Brick.Widgets.Core
  ( Widget
  , (<=>)
  , str
  )

data St =
    St { _stLastVtyEvent :: Maybe V.Event
       , _stCounter :: Int
       }

makeLenses ''St

data CustomEvent = VtyEvent V.Event
                 | Counter

drawUI :: St -> [Widget]
drawUI st = [a]
    where
        a = (str $ "Last Vty event: " <> (show $ st^.stLastVtyEvent))
            <=>
            (str $ "Counter value is: " <> (show $ st^.stCounter))

appEvent :: St -> CustomEvent -> EventM (Next St)
appEvent st e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt st
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
        , appStartEvent = return
        , appAttrMap = def
        , appMakeVtyEvent = VtyEvent
        }

main :: IO ()
main = do
    chan <- newChan

    forkIO $ forever $ do
        writeChan chan Counter
        threadDelay 1000000

    void $ customMain (V.mkVty def) chan theApp initialState
