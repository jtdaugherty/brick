{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Graphics.Vty

import Brick.Main
import Brick.Util
import Brick.Markup
import Brick.AttrMap
import Brick.Widgets.Core
import Data.Text.Markup ((@@))

ui :: Widget
ui = m1 <=> m2
    where
        m1 = markup $ ("Hello" @@ fg blue) <> ", " <> ("world!" @@ fg red)
        m2 = markup $ ("Hello" @? "keyword1") <> ", " <> ("world!" @? "keyword2")

theMap :: AttrMap
theMap = attrMap defAttr
    [ ("keyword1",      fg magenta)
    , ("keyword2",      white `on` blue)
    ]

app :: App () Event
app =
    App { appDraw = const [ui]
        , appHandleEvent = const halt
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        , appMakeVtyEvent = id
        }

main :: IO ()
main = defaultMain app ()
