{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Graphics.Vty
  ( Attr, white, blue, cyan, green, red, yellow
  , black, withURL
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
  , hyperlink
  , modifyDefAttr
  )
import Brick.Util (on, fg)
import Brick.AttrMap (attrMap, AttrMap, attrName)

ui :: Widget n
ui =
    vBox [ str "This text uses the global default attribute."
         , withAttr (attrName "foundFull") $
           str "Specifying an attribute name means we look it up in the attribute tree."
         , withAttr (attrName "foundFgOnly") $
           str "When we find a value, we merge it with its parent in the attribute"
           <=> str "name tree all the way to the root (the global default)."
         , withAttr (attrName "missing") $
           str "A missing attribute name just resumes the search at its parent."
         , withAttr (attrName "general" <> attrName "specific") $
           str "In this way we build complete attribute values by using an inheritance scheme."
         , withAttr (attrName "foundFull") $
           str "You can override everything ..."
         , withAttr (attrName "foundFgOnly") $
           str "... or only what you want to change and inherit the rest."
         , str "Attribute names are assembled with the Monoid append operation to indicate"
         , str "hierarchy levels, e.g. attrName \"window\" <> attrName \"title\"."
         , str " "
         , withAttr (attrName "linked") $
           str "This text is hyperlinked in terminals that support hyperlinking."
         , str " "
         , hyperlink "http://www.google.com/" $
           str "This text is also hyperlinked in terminals that support hyperlinking."
         , str " "
         , modifyDefAttr (`withURL` "http://www.google.com/") $
           str "This text is hyperlinked by modifying the default attribute."
         ]

globalDefault :: Attr
globalDefault = white `on` blue

theMap :: AttrMap
theMap = attrMap globalDefault
    [ (attrName "foundFull",               white `on` green)
    , (attrName "foundFgOnly",             fg red)
    , (attrName "general",                 yellow `on` black)
    , (attrName "general" <> attrName "specific",
                                           fg cyan)
    , (attrName "linked",                  fg yellow `withURL` "http://www.google.com/")
    ]

app :: App () e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = resizeOrQuit
        , appStartEvent = return ()
        , appAttrMap = const theMap
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = defaultMain app ()
