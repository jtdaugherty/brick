{-# LANGUAGE OverloadedStrings #-}
module Brick.Border
  ( border
  , borderWithLabel

  , hBorder
  , hBorderWithLabel
  , vBorder
  )
where

import Brick.Render
import Brick.Center (hCenterWith)
import Brick.Border.Style (BorderStyle(..))

border :: Render -> Render
border = border_ Nothing

borderWithLabel :: String -> Render -> Render
borderWithLabel label = border_ (Just label)

border_ :: Maybe String -> Render -> Render
border_ label wrapped = do
    bs <- getActiveBorderStyle
    let top = txt [bsCornerTL bs] <<+ hBorder_ label +>> txt [bsCornerTR bs]
        bottom = txt [bsCornerBL bs] <<+ hBorder +>> txt [bsCornerBR bs]
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
    hCenterWith (bsHorizontal bs) msg
    where
        msg = maybe (txt "") txt label

vBorder :: Render
vBorder = do
    bs <- getActiveBorderStyle
    vFill (bsVertical bs)
