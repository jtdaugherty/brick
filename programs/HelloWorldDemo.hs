{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick.Main
import Brick.Widgets.Core

ui :: Widget
ui = "Hello, world!"

main :: IO ()
main = simpleMain ui
