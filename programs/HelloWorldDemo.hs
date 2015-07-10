{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick.Main (simpleMain)
import Brick.Widgets.Core (Widget)

ui :: Widget
ui = "Hello, world!"

main :: IO ()
main = simpleMain ui
