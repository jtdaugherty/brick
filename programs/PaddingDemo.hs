{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Default
import qualified Graphics.Vty as V

import Brick.Main (App(..), neverShowCursor, resizeOrQuit, defaultMain)
import Brick.Types
  ( Widget
  , Padding(..)
  )
import Brick.Widgets.Core
  ( vBox
  , hBox
  , str
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
    vBox [ hBox [ padLeft Max $ vCenter $ str "Left-padded"
                , B.vBorder
                , padRight Max $ vCenter $ str "Right-padded"
                ]
         , B.hBorder
         , hBox [ padTop Max $ hCenter $ str "Top-padded"
                , B.vBorder
                , padBottom Max $ hCenter $ str "Bottom-padded"
                ]
         , B.hBorder
         , hBox [ padLeftRight 2 $ str "Padded by 2 on left/right"
                , B.vBorder
                , vBox [ padTopBottom 1 $ str "Padded by 1 on top/bottom"
                       , B.hBorder
                       ]
                ]
         , B.hBorder
         , padAll 2 $ str "Padded by 2 on all sides"
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
