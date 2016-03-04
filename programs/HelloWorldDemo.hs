module Main where

import Brick

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
