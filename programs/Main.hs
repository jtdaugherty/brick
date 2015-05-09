{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { focus :: FocusRing
       , stEditor :: Editor
       }

eName :: Name
eName = Name "edit"

drawUI :: St -> [Widget]
drawUI st = [top]
    where
        top = vBox [ "Top"
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
        ev -> return $ st { stEditor = editEvent ev (stEditor st) }

initialState :: St
initialState =
    St { focus = focusRing [eName]
       , stEditor = editor eName ""
       }

app :: App St
app =
    def { appDraw = drawUI
        , appChooseCursor = focusRingCursor focus
        , appHandleEvent = handleEvent
        }

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty app initialState
