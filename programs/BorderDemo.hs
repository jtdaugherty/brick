{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Data.Monoid
import qualified Data.Text as T
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Util (fg, bg, on)
import qualified Brick.AttrMap as A
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<=>)
  , (<+>)
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  , str
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
    txt $ "  " <> styleName <> " style  "

borderMappings :: [(A.AttrName, V.Attr)]
borderMappings =
    [ (B.borderAttr,         V.yellow `on` V.black)
    , (B.vBorderAttr,        V.green `on` V.red)
    , (B.hBorderAttr,        V.white `on` V.green)
    , (B.hBorderLabelAttr,   fg V.blue)
    , (B.tlCornerAttr,       bg V.red)
    , (B.trCornerAttr,       bg V.blue)
    , (B.blCornerAttr,       bg V.yellow)
    , (B.brCornerAttr,       bg V.green)
    ]

colorDemo :: Widget ()
colorDemo =
    updateAttrMap (A.applyAttrMappings borderMappings) $
    B.borderWithLabel (str "title") $
    hLimit 20 $
    vLimit 5 $
    C.center $
    str "colors!"

ui :: Widget ()
ui =
    hBox borderDemos
    <=> B.hBorder
    <=> colorDemo
    <=> B.hBorderWithLabel (str "horizontal border label")
    <=> (C.center (str "Left of vertical border")
         <+> B.vBorder
         <+> C.center (str "Right of vertical border"))

main :: IO ()
main = M.simpleMain ui
