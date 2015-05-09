{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe
import Data.Monoid
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { msg :: String
       , cursorName :: Name
       }

drawUI :: St -> Widget
drawUI st =
    hBox [ vBox [ "-- header --" `withNamedCursor` (Name "bar", Location (0, 0))
                , txt $ "-- " <> msg st <> " --"
                ] `withAttr` (fg red)
         , vBorder '|'
         , "stuff things" `withNamedCursor` (Name "foo", Location (0, 0))
         ]

handleEvent :: Event -> St -> Either ExitCode St
handleEvent e st =
    case e of
        EvKey KEsc [] -> Left ExitSuccess
        EvKey (KChar '\t') [] -> Right $ st { cursorName = case cursorName st of
                                                             Name "foo" -> Name "bar"
                                                             _ -> Name "foo"
                                            }
        EvKey (KChar c) [] -> Right $ st { msg = msg st ++ [c] }
        _ -> Right st

pickCursor :: St -> [CursorLocation] -> Maybe CursorLocation
pickCursor st ls =
    listToMaybe $ filter (\cl -> cursorLocationName cl == Just (cursorName st)) ls

initialState :: St
initialState =
    St { msg = ""
       , cursorName = Name "foo"
       }

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty drawUI pickCursor handleEvent initialState
