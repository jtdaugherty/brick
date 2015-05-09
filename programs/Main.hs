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

data FocusRing = FocusRingEmpty
               | FocusRingNonemtpy [Name] Int

focusRing :: [Name] -> FocusRing
focusRing [] = FocusRingEmpty
focusRing names = FocusRingNonemtpy names 0

focusNext :: FocusRing -> FocusRing
focusNext FocusRingEmpty = FocusRingEmpty
focusNext (FocusRingNonemtpy ns i) = FocusRingNonemtpy ns i'
    where
        i' = (i + 1) `mod` (length ns)

focusPrev :: FocusRing -> FocusRing
focusPrev FocusRingEmpty = FocusRingEmpty
focusPrev (FocusRingNonemtpy ns i) = FocusRingNonemtpy ns i'
    where
        i' = (i + (length ns) - 1) `mod` (length ns)

focusGetCurrent :: FocusRing -> Maybe Name
focusGetCurrent FocusRingEmpty = Nothing
focusGetCurrent (FocusRingNonemtpy ns i) = Just $ ns !! i

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
