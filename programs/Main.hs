{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { focus :: FocusRing
       , stEditor :: Editor
       }

drawUI :: St -> Widget
drawUI st =
    vBox [ hLimit 15 $ edit (stEditor st) `withAttr` (cyan `on` blue)
         , hBorder '-'
         , "stuff and things"
         ]

handleEvent :: Event -> St -> Either ExitCode St
handleEvent e st =
    case e of
        EvKey KEsc [] -> Left ExitSuccess
        ev -> Right $ st { stEditor = editEvent ev (stEditor st) }

pickCursor :: St -> [CursorLocation] -> Maybe CursorLocation
pickCursor st ls =
    listToMaybe $ filter (\cl -> cursorLocationName cl == (focusGetCurrent $ focus st)) ls

initialState :: St
initialState =
    let eName = Name "edit"
    in St { focus = focusRing [eName]
          , stEditor = editor eName ""
          }

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty drawUI pickCursor handleEvent initialState
