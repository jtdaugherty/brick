{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { msg :: String
       , focus :: FocusRing
       }

drawUI :: St -> Widget
drawUI st =
    hBox [ hLimit 25 $ vBox [ "-- header --"
                            , (txt (msg st)) `withNamedCursor` (Name "bar", Location (length $ msg st, 0))
                            ] `withAttr` (fg red)
         , vBorder '|'
         , "stuff things" `withNamedCursor` (Name "foo", Location (0, 0))
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
    St { msg = ""
       , focus = focusRing [Name "foo", Name "bar"]
       }

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty drawUI pickCursor handleEvent initialState
