-- | This module provides combinators for centering other widgets.
module Brick.Widgets.Center
  ( -- * Centering horizontally
    hCenter
  , hCenterWith
  , hCenterLayer
  -- * Centering vertically
  , vCenter
  , vCenterWith
  , vCenterLayer
  -- * Centering both horizontally and vertically
  , center
  , centerWith
  , centerLayer
  -- * Centering about an arbitrary origin
  , centerAbout
  )
where

import Lens.Micro ((^.), (&), (.~), to)
import Data.Maybe (fromMaybe)
import Graphics.Vty (imageWidth, imageHeight, horizCat, charFill, vertCat,
                    translateX, translateY)

import Brick.Types
import Brick.Widgets.Core

-- | Center the specified widget horizontally. Consumes all available
-- horizontal space.
hCenter :: Widget n -> Widget n
hCenter = hCenterWith Nothing

-- | Center the specified widget horizontally using a Vty image
-- translation. Consumes all available horizontal space. Unlike hCenter,
-- this does not fill the surrounding space so it is suitable for use
-- as a layer. Layers underneath this widget will be visible in regions
-- surrounding the centered widget.
hCenterLayer :: Widget n -> Widget n
hCenterLayer p =
    Widget Greedy (vSize p) $ do
        result <- render p
        c <- getContext
        let rWidth = result^.imageL.to imageWidth
            leftPaddingAmount = max 0 $ (c^.availWidthL - rWidth) `div` 2
            paddedImage = translateX leftPaddingAmount $ result^.imageL
            off = Location (leftPaddingAmount, 0)
        if leftPaddingAmount == 0 then
            return result else
            return $ addResultOffset off
                   $ result & imageL .~ paddedImage

-- | Center the specified widget horizontally. Consumes all available
-- horizontal space. Uses the specified character to fill in the space
-- to either side of the centered widget (defaults to space).
hCenterWith :: Maybe Char -> Widget n -> Widget n
hCenterWith mChar p =
    let ch = fromMaybe ' ' mChar
    in Widget Greedy (vSize p) $ do
           result <- render p
           c <- getContext
           let rWidth = result^.imageL.to imageWidth
               rHeight = result^.imageL.to imageHeight
               remainder = max 0 $ c^.availWidthL - (leftPaddingAmount * 2)
               leftPaddingAmount = max 0 $ (c^.availWidthL - rWidth) `div` 2
               rightPaddingAmount = max 0 $ leftPaddingAmount + remainder
               leftPadding = charFill (c^.attrL) ch leftPaddingAmount rHeight
               rightPadding = charFill (c^.attrL) ch rightPaddingAmount rHeight
               paddedImage = horizCat [ leftPadding
                                      , result^.imageL
                                      , rightPadding
                                      ]
               off = Location (leftPaddingAmount, 0)
           if leftPaddingAmount == 0 && rightPaddingAmount == 0 then
               return result else
               return $ addResultOffset off
                      $ result & imageL .~ paddedImage

-- | Center a widget vertically.  Consumes all vertical space.
vCenter :: Widget n -> Widget n
vCenter = vCenterWith Nothing

-- | Center the specified widget vertically using a Vty image
-- translation. Consumes all available vertical space. Unlike vCenter,
-- this does not fill the surrounding space so it is suitable for use
-- as a layer. Layers underneath this widget will be visible in regions
-- surrounding the centered widget.
vCenterLayer :: Widget n -> Widget n
vCenterLayer p =
    Widget (hSize p) Greedy $ do
        result <- render p
        c <- getContext
        let rHeight = result^.imageL.to imageHeight
            topPaddingAmount = max 0 $ (c^.availHeightL - rHeight) `div` 2
            paddedImage = translateY topPaddingAmount $ result^.imageL
            off = Location (0, topPaddingAmount)
        if topPaddingAmount == 0 then
            return result else
            return $ addResultOffset off
                   $ result & imageL .~ paddedImage

-- | Center a widget vertically. Consumes all vertical space. Uses the
-- specified character to fill in the space above and below the centered
-- widget (defaults to space).
vCenterWith :: Maybe Char -> Widget n -> Widget n
vCenterWith mChar p =
    let ch = fromMaybe ' ' mChar
    in Widget (hSize p) Greedy $ do
           result <- render p
           c <- getContext
           let rWidth = result^.imageL.to imageWidth
               rHeight = result^.imageL.to imageHeight
               remainder = max 0 $ c^.availHeightL - (topPaddingAmount * 2)
               topPaddingAmount = max 0 $ (c^.availHeightL - rHeight) `div` 2
               bottomPaddingAmount = max 0 $ topPaddingAmount + remainder
               topPadding = charFill (c^.attrL) ch rWidth topPaddingAmount
               bottomPadding = charFill (c^.attrL) ch rWidth bottomPaddingAmount
               paddedImage = vertCat [ topPadding
                                     , result^.imageL
                                     , bottomPadding
                                     ]
               off = Location (0, topPaddingAmount)
           if topPaddingAmount == 0 && bottomPaddingAmount == 0 then
               return result else
               return $ addResultOffset off
                      $ result & imageL .~ paddedImage

-- | Center a widget both vertically and horizontally. Consumes all
-- available vertical and horizontal space.
center :: Widget n -> Widget n
center = centerWith Nothing

-- | Center a widget both vertically and horizontally. Consumes all
-- available vertical and horizontal space. Uses the specified character
-- to fill in the space around the centered widget (defaults to space).
centerWith :: Maybe Char -> Widget n -> Widget n
centerWith c = vCenterWith c . hCenterWith c

-- | Center a widget both vertically and horizontally using a Vty image
-- translation. Consumes all available vertical and horizontal space.
-- Unlike center, this does not fill in the surrounding space with a
-- character so it is usable as a layer. Any widget underneath this one
-- will be visible in the region surrounding the centered widget.
centerLayer :: Widget n -> Widget n
centerLayer = vCenterLayer . hCenterLayer

-- | Center the widget horizontally and vertically about the specified
-- origin.
centerAbout :: Location -> Widget n -> Widget n
centerAbout l p =
    Widget Greedy Greedy $ do
      -- Compute translation offset so that loc is in the middle of the
      -- rendering area
      c <- getContext
      let centerW = c^.availWidthL `div` 2
          centerH = c^.availHeightL `div` 2
          off = Location ( centerW - l^.locationColumnL
                         , centerH - l^.locationRowL
                         )
      result <- render $ translateBy off p

      -- Pad the result so it consumes available space
      let rightPaddingAmt = max 0 $ c^.availWidthL - imageWidth (result^.imageL)
          bottomPaddingAmt = max 0 $ c^.availHeightL - imageHeight (result^.imageL)
          rightPadding = charFill (c^.attrL) ' ' rightPaddingAmt (imageHeight $ result^.imageL)
          bottomPadding = charFill (c^.attrL) ' ' (imageWidth $ result^.imageL) bottomPaddingAmt
          paddedImg = horizCat [vertCat [result^.imageL, bottomPadding], rightPadding]

      return $ result & imageL .~ paddedImg
