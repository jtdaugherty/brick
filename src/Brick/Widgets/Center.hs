-- | This module provides combinators for centering other widgets.
module Brick.Widgets.Center
  ( -- * Centering horizontally
    hCenter
  , hCenterWith
  -- * Centering vertically
  , vCenter
  , vCenterWith
  -- * Centering both horizontally and vertically
  , center
  , centerWith
  -- * Centering about an arbitrary origin
  , centerAbout
  )
where

import Control.Lens ((^.), (&), (.~), to)
import Graphics.Vty (imageWidth, imageHeight, horizCat, charFill, vertCat)

import Brick.Types
import Brick.Widgets.Core

-- | Center the specified widget horizontally. Consumes all available
-- horizontal space.
hCenter :: Widget -> Widget
hCenter = hCenterWith Nothing

-- | Center the specified widget horizontally. Consumes all available
-- horizontal space. Uses the specified character to fill in the space
-- to either side of the centered widget (defaults to space).
hCenterWith :: Maybe Char -> Widget -> Widget
hCenterWith mChar p =
    let ch = maybe ' ' id mChar
    in Widget Greedy (vSize p) $ do
           result <- render p
           c <- getContext
           let rWidth = result^.imageL.to imageWidth
               rHeight = result^.imageL.to imageHeight
               remainder = c^.availWidthL - (leftPaddingAmount * 2)
               leftPaddingAmount = (c^.availWidthL - rWidth) `div` 2
               rightPaddingAmount = leftPaddingAmount + remainder
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
vCenter :: Widget -> Widget
vCenter = vCenterWith Nothing

-- | Center a widget vertically. Consumes all vertical space. Uses the
-- specified character to fill in the space above and below the centered
-- widget (defaults to space).
vCenterWith :: Maybe Char -> Widget -> Widget
vCenterWith mChar p =
    let ch = maybe ' ' id mChar
    in Widget (hSize p) Greedy $ do
           result <- render p
           c <- getContext
           let rWidth = result^.imageL.to imageWidth
               rHeight = result^.imageL.to imageHeight
               remainder = c^.availHeightL - (topPaddingAmount * 2)
               topPaddingAmount = (c^.availHeightL - rHeight) `div` 2
               bottomPaddingAmount = topPaddingAmount + remainder
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
center :: Widget -> Widget
center = centerWith Nothing

-- | Center a widget both vertically and horizontally. Consumes all
-- available vertical and horizontal space. Uses the specified character
-- to fill in the space around the centered widget (defaults to space).
centerWith :: Maybe Char -> Widget -> Widget
centerWith c = vCenterWith c . hCenterWith c

-- | Center the widget horizontally and vertically about the specified
-- origin.
centerAbout :: Location -> Widget -> Widget
centerAbout l p =
    Widget Greedy Greedy $ do
      -- Compute translation offset so that loc is in the middle of the
      -- rendering area
      c <- getContext
      let centerW = c^.availWidthL `div` 2
          centerH = c^.availHeightL `div` 2
          off = Location ( centerW - l^.columnL
                         , centerH - l^.rowL
                         )
      render $ translateBy off p
