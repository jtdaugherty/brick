{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { msg :: String
       , focus :: FocusRing
       , stEditor :: Editor
       }

drawUI :: St -> Widget
drawUI st =
    let ew = edit (stEditor st)
    in vBox [ ew `withAttr` (cyan `on` blue)
            , hBorder '-'
            , "stuff and things"
            ]

handleEvent :: Event -> St -> Either ExitCode St
handleEvent e st =
    case e of
        EvKey KEsc [] -> Left ExitSuccess
        EvKey (KChar '\t') [] -> Right $ st { focus = focusNext $ focus st }
        EvKey (KChar c) [] -> Right $ st { msg = msg st ++ [c] }
        _ -> Right st

pickCursor :: St -> [CursorLocation] -> Maybe CursorLocation
pickCursor st ls =
    listToMaybe $ filter (\cl -> cursorLocationName cl == (focusGetCurrent $ focus st)) ls

initialState :: St
initialState =
    let eName = Name "edit"
    St { msg = ""
       , focus = focusRing [eName]
       , stEditor = editor eName ""
       }

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty drawUI pickCursor handleEvent initialState
