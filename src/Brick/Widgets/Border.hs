{-# LANGUAGE OverloadedStrings #-}
-- | This module provides border widgets: vertical borders, horizontal
-- borders, and a box border wrapper widget.
module Brick.Widgets.Border
  ( -- * Border wrapper
    border
  , borderWithLabel

  -- * Horizontal border
  , hBorder
  , hBorderWithLabel

  -- * Vertical border
  , vBorder

  -- * Drawing single border elements
  , borderElem

  -- * Border attribute names
  , borderAttr
  , vBorderAttr
  , hBorderAttr
  , hBorderLabelAttr
  , tlCornerAttr
  , trCornerAttr
  , blCornerAttr
  , brCornerAttr
  )
where

import Control.Lens ((^.), to)
import Data.Monoid ((<>))
import Graphics.Vty (imageHeight, imageWidth)

import Brick.AttrMap
import Brick.Widgets.Core
import Brick.Widgets.Center (hCenterWith)
import Brick.Widgets.Border.Style (BorderStyle(..))

-- | The top-level border attribute name.
borderAttr :: AttrName
borderAttr = "border"

-- | The vertical border attribute name.
vBorderAttr :: AttrName
vBorderAttr = borderAttr <> "vertical"

-- | The horizontal border attribute name.
hBorderAttr :: AttrName
hBorderAttr = borderAttr <> "horizontal"

-- | The attribute used for horizontal border labels.
hBorderLabelAttr :: AttrName
hBorderLabelAttr = hBorderAttr <> "label"

-- | The attribute used for border box top-left corners.
tlCornerAttr :: AttrName
tlCornerAttr = borderAttr <> "corner" <> "tl"

-- | The attribute used for border box top-right corners.
trCornerAttr :: AttrName
trCornerAttr = borderAttr <> "corner" <> "tr"

-- | The attribute used for border box bottom-left corners.
blCornerAttr :: AttrName
blCornerAttr = borderAttr <> "corner" <> "bl"

-- | The attribute used for border box bottom-right corners.
brCornerAttr :: AttrName
brCornerAttr = borderAttr <> "corner" <> "br"

-- | Draw the specified border element using the active border style
-- using 'borderAttr'.
borderElem :: (BorderStyle -> Char) -> Widget
borderElem f =
    Widget Fixed Fixed $ do
      bs <- getActiveBorderStyle
      render $ withAttr borderAttr $ str [f bs]

-- | Put a border around the specified widget.
border :: Widget -> Widget
border = border_ Nothing

-- | Put a border around the specified widget with the specified label
-- widget placed in the middle of the top horizontal border.
borderWithLabel :: Widget
                -- ^ The label widget
                -> Widget
                -- ^ The widget to put a border around
                -> Widget
borderWithLabel label = border_ (Just label)

border_ :: Maybe Widget -> Widget -> Widget
border_ label wrapped =
    Widget (hSize wrapped) (vSize wrapped) $ do
      bs <- getActiveBorderStyle
      c <- getContext

      middleResult <- render $ hLimit (c^.availW - 2)
                             $ vLimit (c^.availH - 2)
                             $ wrapped

      let top = (withAttr tlCornerAttr $ str [bsCornerTL bs])
                <+> hBorder_ label <+>
                (withAttr trCornerAttr $ str [bsCornerTR bs])
          bottom = (withAttr blCornerAttr $ str [bsCornerBL bs])
                   <+> hBorder <+>
                   (withAttr brCornerAttr $ str [bsCornerBR bs])
          middle = vBorder <+> (Widget Fixed Fixed $ return middleResult) <+> vBorder
          total = top <=> middle <=> bottom

      render $ hLimit (middleResult^.image.to imageWidth + 2)
             $ vLimit (middleResult^.image.to imageHeight + 2)
             $ total

-- | A horizontal border.  Fills all horizontal space.
hBorder :: Widget
hBorder = hBorder_ Nothing

-- | A horizontal border with a label placed in the center of the
-- border. Fills all horizontal space.
hBorderWithLabel :: Widget
                 -- ^ The label widget
                 -> Widget
hBorderWithLabel label = hBorder_ (Just label)

hBorder_ :: Maybe Widget -> Widget
hBorder_ label =
    Widget Unlimited Fixed $ do
      bs <- getActiveBorderStyle
      render $ vLimit 1 $ withAttr hBorderAttr $ hCenterWith (Just $ bsHorizontal bs) msg
      where
          msg = maybe (txt "") (withAttr hBorderLabelAttr) label

-- | A vertical border.  Fills all vertical space.
vBorder :: Widget
vBorder =
    Widget Fixed Unlimited $ do
      bs <- getActiveBorderStyle
      render $ hLimit 1 $ withAttr vBorderAttr $ fill (bsVertical bs)
