module Main where

import Brick

ui :: Widget ()
ui = vBox [ str "This text is at the top."
          , fill ' '
          , str "This text is at the bottom."
          ]

main :: IO ()
main = simpleMain ui
