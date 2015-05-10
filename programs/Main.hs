{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { stEditor :: Editor
       , trans :: Location
       }

eName :: Name
eName = Name "edit"

drawUI :: St -> [Widget]
drawUI st = [top]
    where
        top = translated (trans st) $
              bordered $
              hLimit 40 $
              vBox [ "Top"
                   , hBorder '-'
                   , hBox [ " Edit: "
                          , hLimit 20 $ edit (stEditor st) `withAttr` (cyan `on` blue)
                          ]
                   ]

handleEvent :: Event -> St -> IO St
handleEvent e st =
    case e of
        EvKey KEsc [] -> exitSuccess
        EvKey KEnter [] -> error $ editStr $ stEditor st
        EvKey KLeft [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (-1, 0)) }
        EvKey KRight [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (1, 0)) }
        EvKey KUp [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (0, -1)) }
        EvKey KDown [MCtrl] -> return $ st { trans = trans st `locOffset` (Location (0, 1)) }
        ev -> return $ st { stEditor = editEvent ev (stEditor st) }

initialState :: St
initialState =
    St { stEditor = editor eName ""
       , trans = Location (0, 0)
       }

theApp :: App St Event
theApp =
    def { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        }

main :: IO ()
main = defaultMain theApp initialState
