module Main where

import Brick (Widget, simpleMain, (<+>), str, withBorderStyle, joinBorders)
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, vBorder)
import Brick.Widgets.Border.Style (unicode)

ui :: Widget ()
ui =
    joinBorders $
    withBorderStyle unicode $
    borderWithLabel (str "Hello!") $
    (center (str "Left") <+> vBorder <+> center (str "Right"))

main :: IO ()
main = simpleMain ui
