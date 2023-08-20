{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif

import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Util (fg, on)
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , withAttr
  , vLimit
  , hLimit
  , hBox
  , vBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
  , padLeftRight
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS

styles :: [(T.Text, BS.BorderStyle)]
styles =
    [ ("ascii", BS.ascii)
    , ("unicode", BS.unicode)
    , ("unicode bold", BS.unicodeBold)
    , ("unicode rounded", BS.unicodeRounded)
    , ("custom", custom)
    , ("from 'x'", BS.borderStyleFromChar 'x')
    ]

custom :: BS.BorderStyle
custom =
    BS.BorderStyle { BS.bsCornerTL = '/'
                   , BS.bsCornerTR = '\\'
                   , BS.bsCornerBR = '/'
                   , BS.bsCornerBL = '\\'
                   , BS.bsIntersectFull = '.'
                   , BS.bsIntersectL = '.'
                   , BS.bsIntersectR = '.'
                   , BS.bsIntersectT = '.'
                   , BS.bsIntersectB = '.'
                   , BS.bsHorizontal = '*'
                   , BS.bsVertical = '!'
                   }

borderDemos :: [Widget ()]
borderDemos = mkBorderDemo <$> styles

mkBorderDemo :: (T.Text, BS.BorderStyle) -> Widget ()
mkBorderDemo (styleName, sty) =
    withBorderStyle sty $
    B.borderWithLabel (str "label") $
    vLimit 5 $
    C.vCenter $
    padLeftRight 2 $
    txt $ styleName <> " style"

titleAttr :: A.AttrName
titleAttr = A.attrName "title"

attrs :: [(A.AttrName, V.Attr)]
attrs =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (B.vBorderAttr,        fg V.cyan)
    , (B.hBorderAttr,        fg V.magenta)
    , (titleAttr,            fg V.cyan)
    ]

colorDemo :: Widget ()
colorDemo =
    updateAttrMap (A.applyAttrMappings attrs) $
    B.borderWithLabel (withAttr titleAttr $ str "title") $
    hLimit 20 $
    vLimit 5 $
    C.center $
    str "colors!"

ui :: Widget ()
ui =
    vBox [ hBox borderDemos
         , B.hBorder
         , colorDemo
         , B.hBorderWithLabel (str "horizontal border label")
         , (C.center (str "Left of vertical border")
             <+> B.vBorder
             <+> C.center (str "Right of vertical border"))
         ]

main :: IO ()
main = M.simpleMain ui
