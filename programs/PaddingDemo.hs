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
    vBox [ hBox [ padLeft $ vCenter "Left-padded"
                , vBorder
                , padRight $ vCenter "Right-padded"
                ]
         , hBorder
         , hBox [ padTop $ hCenter "Top-padded"
                , vBorder
                , padBottom $ hCenter "Bottom-padded"
                ]
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
