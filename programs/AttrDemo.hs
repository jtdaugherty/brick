{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Graphics.Vty

import Brick.Main
import Brick.Widgets.Core
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap)

ui :: Widget
ui =
    vBox [ "This text uses the global default attribute."
         , withAttr "foundFull"
           "Specifying an attribute name means we look it up in the attribute tree."
         , withAttr "foundFgOnly"
           ("When we find a value, we merge it with its parent in the attribute"
           <=> "name tree all the way to the root (the global default).")
         , withAttr "missing"
           "A missing attribute name just resumes the search at its parent."
         , withAttr ("general" <> "specific")
           "In this way we build complete attribute values by using an inheritance scheme."
         , withAttr "foundFull"
           "You can override everything ..."
         , withAttr "foundFgOnly"
           "... or only you want to change and inherit the rest."
         , "Attribute names are assembled with the Monoid append operation to indicate"
         , "hierarchy levels, e.g. \"window\" <> \"title\"."
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

app :: App () Event
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        , appMakeVtyEvent = id
        }

main :: IO ()
main = defaultMain app ()
