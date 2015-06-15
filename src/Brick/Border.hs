{-# LANGUAGE OverloadedStrings #-}
module Brick.Border
  ( border
  , borderWithLabel

  , hBorder
  , hBorderWithLabel
  , vBorder

  , borderAttr
  , vBorderAttr
  , hBorderAttr
  , tlCornerAttr
  , trCornerAttr
  , blCornerAttr
  , brCornerAttr
  )
where

import Data.Monoid ((<>))

import Brick.Render
import Brick.AttrMap
import Brick.Center (hCenterWith)
import Brick.Border.Style (BorderStyle(..))

borderAttr :: AttrName
borderAttr = "border"

vBorderAttr :: AttrName
vBorderAttr = borderAttr <> "vertical"

hBorderAttr :: AttrName
hBorderAttr = borderAttr <> "horizontal"

tlCornerAttr :: AttrName
tlCornerAttr = borderAttr <> "corner" <> "tl"

trCornerAttr :: AttrName
trCornerAttr = borderAttr <> "corner" <> "tr"

blCornerAttr :: AttrName
blCornerAttr = borderAttr <> "corner" <> "bl"

brCornerAttr :: AttrName
brCornerAttr = borderAttr <> "corner" <> "br"

border :: Render -> Render
border = border_ Nothing

borderWithLabel :: String -> Render -> Render
borderWithLabel label = border_ (Just label)

border_ :: Maybe String -> Render -> Render
border_ label wrapped = do
    bs <- getActiveBorderStyle
    let top = (withAttrName tlCornerAttr $ txt [bsCornerTL bs])
              <<+ hBorder_ label +>>
              (withAttrName trCornerAttr $ txt [bsCornerTR bs])
        bottom = (withAttrName blCornerAttr $ txt [bsCornerBL bs])
                 <<+ hBorder +>>
                 (withAttrName brCornerAttr $ txt [bsCornerBR bs])
        middle = vBorder +>> wrapped <<+ vBorder
        total = top =>> middle <<= bottom
    total

hBorder :: Render
hBorder = hBorder_ Nothing

hBorderWithLabel :: String -> Render
hBorderWithLabel label = hBorder_ (Just label)

hBorder_ :: Maybe String -> Render
hBorder_ label = do
    bs <- getActiveBorderStyle
    withAttrName hBorderAttr $ hCenterWith (Just $ bsHorizontal bs) msg
    where
        msg = maybe (txt "") txt label

vBorder :: Render
vBorder = do
    bs <- getActiveBorderStyle
    withAttrName vBorderAttr $ vFill (bsVertical bs)
