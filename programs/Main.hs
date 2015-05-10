{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Data.Default
import Data.Monoid
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { focus :: FocusRing
       , stEditor :: Editor
       , trans :: Location
       , counter :: Int
       }

eName :: Name
eName = Name "edit"

drawUI :: St -> [Widget]
drawUI st = [top]
    where
        top = translated (trans st) $
              bordered $
              hLimit 40 $
              vBox [ txt $ "Top (counter: " <> show (counter st) <> ")"
                   , hBorder '-'
                   , hBox [ " Edit: "
                          , hLimit 20 $ edit (stEditor st) `withAttr` (cyan `on` blue)
                          ]
                   ]

handleEvent :: MyEvent -> St -> IO St
handleEvent e st =
    case e of
        VtyEvent vtyEv ->
            case vtyEv of
                EvKey KEsc [] -> exitSuccess
                EvKey KEnter [] -> error $ editStr $ stEditor st
                EvKey KLeft [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (-1, 0)) }
                EvKey KRight [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (1, 0)) }
                EvKey KUp [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (0, -1)) }
                EvKey KDown [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (0, 1)) }
                ev -> return $ st { stEditor = editEvent ev (stEditor st) }
        CounterUpdate i -> return $ st { counter = i }

initialState :: St
initialState =
    St { focus = focusRing [eName]
       , stEditor = editor eName ""
       , trans = Location (0, 0)
       , counter = 0
       }

data MyEvent = VtyEvent Event
             | CounterUpdate Int

theApp :: App St MyEvent
theApp =
    def { appDraw = drawUI
        , appChooseCursor = focusRingCursor focus
        , appHandleEvent = handleEvent
        }

updateThread :: Chan MyEvent -> IO ()
updateThread chan = do
    let run i = do
                 writeChan chan $ CounterUpdate i
                 threadDelay 1000000
                 run $ i + 1
    run 0

main :: IO ()
main = do
    chan <- newChan
    withVty (mkVty def) $ \vty -> do
        forkIO $ supplyVtyEvents vty VtyEvent chan
        forkIO $ updateThread chan
        runVty vty chan theApp initialState
