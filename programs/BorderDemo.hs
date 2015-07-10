{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Data.Monoid
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Brick.Main
  ( App(..)
  , simpleMain
  )
import Brick.Util (fg, bg, on)
import Brick.AttrMap
  ( AttrMap
  , AttrName
  , attrMap
  , applyAttrMappings
  )
import Brick.Widgets.Core
  ( Widget
  , (<=>)
  , (<+>)
  , vLimit
  , hLimit
  , hBox
  , updateAttrMap
  , withBorderStyle
  , txt
  )
import Brick.Widgets.Center
  ( vCenter
  , center
  )
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
                   , BS.bsIntersectionFull = '.'
                   , BS.bsIntersectionL = '.'
                   , BS.bsIntersectionR = '.'
                   , BS.bsIntersectionT = '.'
                   , BS.bsIntersectionB = '.'
                   , BS.bsHorizontal = '*'
                   , BS.bsVertical = '!'
                   }

borderDemos :: [Widget]
borderDemos = mkBorderDemo <$> styles

mkBorderDemo :: (T.Text, BS.BorderStyle) -> Widget
mkBorderDemo (styleName, sty) =
    withBorderStyle sty $
    B.borderWithLabel "label" $
    vLimit 5 $
    vCenter $
    txt $ "  " <> styleName <> " style  "

borderMappings :: [(AttrName, V.Attr)]
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

colorDemo :: Widget
colorDemo =
    updateAttrMap (applyAttrMappings borderMappings) $
    B.borderWithLabel "title" $
    hLimit 20 $
    vLimit 5 $
    center $
    "colors!"

ui :: Widget
ui =
    hBox borderDemos
    <=> B.hBorder
    <=> colorDemo
    <=> B.hBorderWithLabel "horizontal border label"
    <=> (center "Left of vertical border"
         <+> B.vBorder
         <+> center "Right of vertical border")

main :: IO ()
main = simpleMain ui
