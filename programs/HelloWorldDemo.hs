module Main where

import Brick.Main (simpleMain)
import Brick.Types (Widget)
import Brick.Widgets.Core (str)

ui :: Widget
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
