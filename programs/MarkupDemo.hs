{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import Brick.Main (App(..), defaultMain, resizeOrQuit, neverShowCursor)
import Brick.Widgets.Core
  ( Widget
  , (<=>)
  )
import Brick.Util (on, fg)
import Brick.Markup (markup, (@?))
import Brick.AttrMap (attrMap, AttrMap)
import Data.Text.Markup ((@@))

ui :: Widget
ui = m1 <=> m2
    where
        m1 = markup $ ("Hello" @@ fg V.blue) <> ", " <> ("world!" @@ fg V.red)
        m2 = markup $ ("Hello" @? "keyword1") <> ", " <> ("world!" @? "keyword2")

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.magenta)
    , ("keyword2",      V.white `on` V.blue)
    ]

app :: App () V.Event
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = neverShowCursor
        , appLiftVtyEvent = id
        }

main :: IO ()
main = defaultMain app ()
