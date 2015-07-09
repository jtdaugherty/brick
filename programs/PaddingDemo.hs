{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default
import Graphics.Vty

import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.Center

ui :: Widget
ui =
    vBox [ hBox [ padLeft Max $ vCenter "Left-padded"
                , vBorder
                , padRight Max $ vCenter "Right-padded"
                ]
         , hBorder
         , hBox [ padTop Max $ hCenter "Top-padded"
                , vBorder
                , padBottom Max $ hCenter "Bottom-padded"
                ]
         , hBorder
         , hBox [ padLeftRight (Pad 2) "Padded by 2 on left/right"
                , vBorder
                , vBox [ padTopBottom (Pad 1) "Padded by 1 on top/bottom"
                       , hBorder
                       ]
                ]
         , hBorder
         , padAll (Pad 2) "Padded by 2 on all sides"
         ]

app :: App () Event
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const def
        , appChooseCursor = neverShowCursor
        , appMakeVtyEvent = id
        }

main :: IO ()
main = defaultMain app ()
