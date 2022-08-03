{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick.Main (App(..), neverShowCursor, resizeOrQuit, defaultMain)
import Brick.Types
  ( Widget
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
  , Padding(..)
  )
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V

ui :: Widget ()
ui =
    vBox [ hBox [ padLeft Max $ C.vCenter $ str "Left-padded"
                , B.vBorder
                , padRight Max $ C.vCenter $ str "Right-padded"
                ]
         , B.hBorder
         , hBox [ padTop Max $ C.hCenter $ str "Top-padded"
                , B.vBorder
                , padBottom Max $ C.hCenter $ str "Bottom-padded"
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

app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = defaultMain app ()
