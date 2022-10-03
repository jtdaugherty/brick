{-# LANGUAGE OverloadedStrings #-}
module Main where

import Brick.Main (App(..), neverShowCursor, resizeOrQuit, defaultMain)
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( vBox
  , hBox
  , txt
  , (<=>)
  , padRight
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy
  , cropLeftTo
  , cropRightTo
  , cropTopTo
  , cropBottomTo
  , Padding(..)
  )
import Brick.Widgets.Border (border)
import Brick.AttrMap (attrMap)
import qualified Graphics.Vty as V

example :: Widget n
example =
    border
    (txt "Example" <=> txt "Widget")

mkExample :: Widget n -> Widget n
mkExample = padRight (Pad 2)

ui :: Widget ()
ui =
    vBox [ txt "Uncropped" <=> example
         , hBox [ mkExample $ txt "cropLeftBy 2"   <=> cropLeftBy 2 example
                , mkExample $ txt "cropRightBy 2"  <=> cropRightBy 2 example
                , mkExample $ txt "cropTopBy 2"    <=> cropTopBy 2 example
                , mkExample $ txt "cropBottomBy 2" <=> cropBottomBy 2 example
                ]
         , hBox [ mkExample $ txt "cropLeftTo 4"   <=> cropLeftTo 4 example
                , mkExample $ txt "cropRightTo 4"  <=> cropRightTo 4 example
                , mkExample $ txt "cropTopTo 1"    <=> cropTopTo 1 example
                , mkExample $ txt "cropBottomTo 1" <=> cropBottomTo 1 example
                ]
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
