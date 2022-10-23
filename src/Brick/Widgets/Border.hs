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
  , hBorderAttr
  , vBorderAttr

  -- * Utility
  , joinableBorder
  )
where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Lens.Micro ((^.), (&), (.~), to)
import Graphics.Vty (imageHeight, imageWidth)

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border.Style (BorderStyle(..))
import Brick.Widgets.Internal (renderDynBorder)
import Data.IMap (Run(..))
import qualified Brick.BorderMap as BM

-- | The top-level border attribute name.
borderAttr :: AttrName
borderAttr = attrName "border"

-- | The horizontal border attribute name. Inherits from 'borderAttr'.
hBorderAttr :: AttrName
hBorderAttr = borderAttr <> attrName "horizontal"

-- | The vertical border attribute name. Inherits from 'borderAttr'.
vBorderAttr :: AttrName
vBorderAttr = borderAttr <> attrName "vertical"

-- | Draw the specified border element using the active border style
-- using 'borderAttr'.
--
-- Does not participate in dynamic borders (due to the difficulty of
-- introspecting on the first argument); consider using 'joinableBorder'
-- instead.
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

      let tl = joinableBorder (Edges False True False True)
          tr = joinableBorder (Edges False True True False)
          bl = joinableBorder (Edges True False False True)
          br = joinableBorder (Edges True False True False)
          top = tl <+> maybe hBorder hBorderWithLabel label <+> tr
          bottom = bl <+> hBorder <+> br
          middle = vBorder <+> (Widget Fixed Fixed $ return middleResult) <+> vBorder
          total = top <=> middle <=> bottom

      render $ hLimit (middleResult^.imageL.to imageWidth + 2)
             $ vLimit (middleResult^.imageL.to imageHeight + 2)
             $ total

-- | A horizontal border. Fills all horizontal space. Draws using
-- 'hBorderAttr'.
hBorder :: Widget n
hBorder =
    withAttr borderAttr $ Widget Greedy Fixed $ do
      ctx <- getContext
      let bs = ctxBorderStyle ctx
          w = availWidth ctx
      db <- dynBorderFromDirections (Edges False False True True)
      let dynBorders = BM.insertH mempty (Run w db)
                     $ BM.emptyCoordinates (Edges 0 0 0 (w-1))
      setDynBorders dynBorders $ render $ withAttr hBorderAttr
                               $ vLimit 1 $ fill (bsHorizontal bs)

-- | A horizontal border with a label placed in the center of the
-- border. Fills all horizontal space.
hBorderWithLabel :: Widget n
                 -- ^ The label widget
                 -> Widget n
hBorderWithLabel label =
    Widget Greedy Fixed $ do
      res <- render $ vLimit 1 label
      render $ hBox [hBorder, Widget Fixed Fixed (return res), hBorder]

-- | A vertical border. Fills all vertical space. Draws using
-- 'vBorderAttr'.
vBorder :: Widget n
vBorder =
    withAttr borderAttr $ Widget Fixed Greedy $ do
      ctx <- getContext
      let bs = ctxBorderStyle ctx
          h = availHeight ctx
      db <- dynBorderFromDirections (Edges True True False False)
      let dynBorders = BM.insertV mempty (Run h db)
                     $ BM.emptyCoordinates (Edges 0 (h-1) 0 0)
      setDynBorders dynBorders $ render $ withAttr vBorderAttr
                               $ hLimit 1 $ fill (bsVertical bs)

-- | Initialize a 'DynBorder'. It will be 'bsDraw'n and 'bsOffer'ing
-- in the given directions to begin with, and accept join offers from
-- all directions. We consult the context to choose the 'dbStyle' and
-- 'dbAttr'.
--
-- This is likely to be useful only for custom widgets that need more
-- complicated dynamic border behavior than 'border', 'vBorder', or
-- 'hBorder' offer.
dynBorderFromDirections :: Edges Bool -> RenderM n DynBorder
dynBorderFromDirections dirs = do
    ctx <- getContext
    return DynBorder
        { dbStyle = ctxBorderStyle ctx
        , dbAttr = attrMapLookup (ctxAttrName ctx) (ctxAttrMap ctx)
        , dbSegments = (\draw -> BorderSegment True draw draw) <$> dirs
        }

-- | Replace the 'Result'\'s dynamic borders with the given one,
-- provided the context says to use dynamic borders at all.
setDynBorders :: BM.BorderMap DynBorder -> RenderM n (Result n) -> RenderM n (Result n)
setDynBorders newBorders act = do
    dyn <- ctxDynBorders <$> getContext
    res <- act
    return $ if dyn
        then res & bordersL .~ newBorders
        else res

-- | A single-character dynamic border that will react to neighboring
-- borders, initially connecting in the given directions.
joinableBorder :: Edges Bool -> Widget n
joinableBorder dirs = withAttr borderAttr . Widget Fixed Fixed $ do
    db <- dynBorderFromDirections dirs
    setDynBorders
        (BM.singleton mempty db)
        (render (raw (renderDynBorder db)))
