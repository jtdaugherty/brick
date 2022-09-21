module Main where

import Brick
import Brick.Widgets.Border

ui :: Widget ()
ui = vBox [ vLimitPercent 20 $ vBox [ str "This text is in the top 20% of the window due to a fill and vLimitPercent."
                                    , fill ' '
                                    , hBorder
                                    ]
          , str "This text is at the bottom with another fill beneath it."
          , fill 'x'
          ]

main :: IO ()
main = simpleMain ui
