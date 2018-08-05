module Main where

import Brick
import Brick.Widgets.Border

ui :: Widget ()
ui = vBox [ vLimitPercent 20 $ vBox [ str "This text is at the top."
                                    , fill ' '
                                    , hBorder
                                    ]
          , str "This text is at the bottom."
          ]

main :: IO ()
main = simpleMain ui
