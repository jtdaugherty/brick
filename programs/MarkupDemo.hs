{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import Brick.Main (App(..), defaultMain, resizeOrQuit, neverShowCursor)
import Brick.Types
  ( Widget
  , Padding(..)
  )
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , padLeft
  )
import Brick.Util (on, fg)
import Brick.Markup (markup, (@?))
import Brick.AttrMap (attrMap, AttrMap)
import Data.Text.Markup ((@@))

ui :: Widget ()
ui = (m1 <=> m2) <+> (padLeft (Pad 1) m3)
    where
        m1 = markup $ ("Hello" @@ fg V.blue) <> ", " <> ("world!" @@ fg V.red)
        m2 = markup $ ("Hello" @? "keyword1") <> ", " <> ("world!" @? "keyword2")
        m3 = markup $ ("Hello," @? "keyword1") <> "\n" <> ("world!" @? "keyword2")

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("keyword1",      fg V.magenta)
    , ("keyword2",      V.white `on` V.blue)
    ]

app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appAttrMap = const theMap
        , appStartEvent = return
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = defaultMain app ()
