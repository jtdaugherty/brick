{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default
import qualified Graphics.Vty as V

import Brick.Main (App(..), neverShowCursor, resizeOrQuit, defaultMain)
import Brick.Widgets.Core
  ( Widget
  , vBox
  , hBox
  , Padding(..)
  , padAll
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padTopBottom
  , padLeftRight
  )
import Brick.Widgets.Border as B
import Brick.Widgets.Center as C

ui :: Widget
ui =
    vBox [ hBox [ padLeft Max $ vCenter "Left-padded"
                , B.vBorder
                , padRight Max $ vCenter "Right-padded"
                ]
         , B.hBorder
         , hBox [ padTop Max $ hCenter "Top-padded"
                , B.vBorder
                , padBottom Max $ hCenter "Bottom-padded"
                ]
         , B.hBorder
         , hBox [ padLeftRight (Pad 2) "Padded by 2 on left/right"
                , B.vBorder
                , vBox [ padTopBottom (Pad 1) "Padded by 1 on top/bottom"
                       , B.hBorder
                       ]
                ]
         , B.hBorder
         , padAll (Pad 2) "Padded by 2 on all sides"
         ]

app :: App () V.Event
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const def
        , appChooseCursor = neverShowCursor
        , appLiftVtyEvent = id
        }

main :: IO ()
main = defaultMain app ()
