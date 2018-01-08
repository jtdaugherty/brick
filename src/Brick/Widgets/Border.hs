{-# LANGUAGE OverloadedStrings #-}
-- | This module provides border widgets: vertical borders, horizontal
-- borders, and a box border wrapper widget. All functions in this
-- module use the rendering context's active 'BorderStyle'; to change
-- the 'BorderStyle', use 'withBorderStyle'.
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

  -- * Attribute names
  , borderAttr
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Lens.Micro ((^.), to)
import Graphics.Vty (imageHeight, imageWidth)

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center (hCenterWith)
import Brick.Widgets.Border.Style (BorderStyle(..))

-- | The top-level border attribute name.
borderAttr :: AttrName
borderAttr = "border"

-- | Draw the specified border element using the active border style
-- using 'borderAttr'.
borderElem :: (BorderStyle -> Char) -> Widget n
borderElem f =
    Widget Fixed Fixed $ do
      bs <- ctxBorderStyle <$> getContext
      render $ withAttr borderAttr $ str [f bs]

-- | Put a border around the specified widget.
border :: Widget n -> Widget n
border = border_ Nothing

-- | Put a border around the specified widget with the specified label
-- widget placed in the middle of the top horizontal border.
--
-- Note that a border will wrap its child widget as tightly as possible,
-- which means that if the child widget is narrower than the label
-- widget, the label widget will be truncated. If you want to avoid
-- this behavior, add a 'fill' or other space-filling wrapper to the
-- bordered widget so that it takes up enough room to make the border
-- horizontally able to avoid truncating the label.
borderWithLabel :: Widget n
                -- ^ The label widget
                -> Widget n
                -- ^ The widget to put a border around
                -> Widget n
borderWithLabel label = border_ (Just label)

border_ :: Maybe (Widget n) -> Widget n -> Widget n
border_ label wrapped =
    Widget (hSize wrapped) (vSize wrapped) $ do
      c <- getContext

      middleResult <- render $ hLimit (c^.availWidthL - 2)
                             $ vLimit (c^.availHeightL - 2)
                             $ wrapped

      let top = (borderElem bsCornerTL) <+> hBorder_ label <+> (borderElem bsCornerTR)
          bottom = (borderElem bsCornerBL) <+> hBorder <+> (borderElem bsCornerBR)
          middle = vBorder <+> (Widget Fixed Fixed $ return middleResult) <+> vBorder
          total = top <=> middle <=> bottom

      render $ hLimit (middleResult^.imageL.to imageWidth + 2)
             $ vLimit (middleResult^.imageL.to imageHeight + 2)
             $ total

-- | A horizontal border.  Fills all horizontal space.
hBorder :: Widget n
hBorder = hBorder_ Nothing

-- | A horizontal border with a label placed in the center of the
-- border. Fills all horizontal space.
hBorderWithLabel :: Widget n
                 -- ^ The label widget
                 -> Widget n
hBorderWithLabel label = hBorder_ (Just label)

hBorder_ :: Maybe (Widget n) -> Widget n
hBorder_ label =
    Widget Greedy Fixed $ do
      bs <- ctxBorderStyle <$> getContext
      let msg = maybe (str [bsHorizontal bs]) id label
      render $ vLimit 1 $ withAttr borderAttr $ hCenterWith (Just $ bsHorizontal bs) msg

-- | A vertical border.  Fills all vertical space.
vBorder :: Widget n
vBorder =
    Widget Fixed Greedy $ do
      bs <- ctxBorderStyle <$> getContext
      render $ hLimit 1 $ withAttr borderAttr $ fill (bsVertical bs)
