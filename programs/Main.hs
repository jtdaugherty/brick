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

drawUI :: St -> Widget
drawUI st =
    vBox [ hLimit 15 $ edit (stEditor st) `withAttr` (cyan `on` blue)
         , hBorder '-'
         , "stuff and things"
         ]

handleEvent :: Event -> St -> IO St
handleEvent e st =
    case e of
        EvKey KEsc [] -> exitSuccess
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
