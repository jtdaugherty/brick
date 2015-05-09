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

eName :: Name
eName = Name "edit"

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
    listToMaybe $ filter isCurrent ls
    where
        isCurrent cl = cursorLocationName cl ==
                       (focusGetCurrent $ focus st)

initialState :: St
initialState =
    St { focus = focusRing [eName]
       , stEditor = editor eName ""
       }

handleResize :: Name -> DisplayRegion -> St -> St
handleResize name size st = error $ show name

main :: IO ()
main = standardIOConfig
       >>= mkVty
       >>= runVty drawUI pickCursor handleEvent handleResize initialState
