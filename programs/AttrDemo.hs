{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Graphics.Vty
  ( Attr, white, blue, cyan, green, red, yellow
  , black
  )

import Brick.Main
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<=>)
  , withAttr
  , vBox
  , str
  )
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)

ui :: Widget n
ui =
    vBox [ str "This text uses the global default attribute."
         , withAttr "foundFull" $
           str "Specifying an attribute name means we look it up in the attribute tree."
         , (withAttr "foundFgOnly" $
           str ("When we find a value, we merge it with its parent in the attribute")
           <=> str "name tree all the way to the root (the global default).")
         , withAttr "missing" $
           str "A missing attribute name just resumes the search at its parent."
         , withAttr ("general" <> "specific") $
           str "In this way we build complete attribute values by using an inheritance scheme."
         , withAttr "foundFull" $
           str "You can override everything ..."
         , withAttr "foundFgOnly" $
           str "... or only you want to change and inherit the rest."
         , str "Attribute names are assembled with the Monoid append operation to indicate"
         , str "hierarchy levels, e.g. \"window\" <> \"title\"."
         ]

globalDefault :: Attr
globalDefault = white `on` blue

theMap :: AttrMap
theMap = attrMap globalDefault
    [ ("foundFull",               white `on` green)
    , ("foundFgOnly",             fg red)
    , ("general",                 yellow `on` black)
    , ("general" <> "specific",   fg cyan)
    ]

app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = defaultMain app ()
