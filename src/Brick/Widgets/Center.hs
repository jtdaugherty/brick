module Brick.Widgets.Center
  ( hCenter
  , hCenterWith
  , vCenter
  , vCenterWith
  , center
  , centerWith
  , centerAbout
  )
where

import Control.Lens ((^.), (&), (.~), to)
import Graphics.Vty (imageWidth, imageHeight, horizCat, charFill, vertCat)

import Brick.Widgets.Core
import Brick.Core

hCenter :: Widget -> Widget
hCenter = hCenterWith Nothing

hCenterWith :: Maybe Char -> Widget -> Widget
hCenterWith mChar p =
    let ch = maybe ' ' id mChar
    in Widget Unlimited (vSize p) $ do
           result <- render p
           c <- getContext
           let rWidth = result^.image.to imageWidth
               rHeight = result^.image.to imageHeight
               remainder = c^.availW - (leftPaddingAmount * 2)
               leftPaddingAmount = (c^.availW - rWidth) `div` 2
               rightPaddingAmount = leftPaddingAmount + remainder
               leftPadding = charFill (c^.attr) ch leftPaddingAmount rHeight
               rightPadding = charFill (c^.attr) ch rightPaddingAmount rHeight
               paddedImage = horizCat [ leftPadding
                                      , result^.image
                                      , rightPadding
                                      ]
               off = Location (leftPaddingAmount, 0)
           if leftPaddingAmount == 0 && rightPaddingAmount == 0 then
               return result else
               return $ addVisibilityOffset off
                      $ addCursorOffset off
                      $ result & image .~ paddedImage

vCenter :: Widget -> Widget
vCenter = vCenterWith Nothing

vCenterWith :: Maybe Char -> Widget -> Widget
vCenterWith mChar p =
    let ch = maybe ' ' id mChar
    in Widget (hSize p) Unlimited $ do
           result <- render p
           c <- getContext
           let rWidth = result^.image.to imageWidth
               rHeight = result^.image.to imageHeight
               remainder = c^.availH - (topPaddingAmount * 2)
               topPaddingAmount = (c^.availH - rHeight) `div` 2
               bottomPaddingAmount = topPaddingAmount + remainder
               topPadding = charFill (c^.attr) ch rWidth topPaddingAmount
               bottomPadding = charFill (c^.attr) ch rWidth bottomPaddingAmount
               paddedImage = vertCat [ topPadding
                                     , result^.image
                                     , bottomPadding
                                     ]
               off = Location (0, topPaddingAmount)
           if topPaddingAmount == 0 && bottomPaddingAmount == 0 then
               return result else
               return $ addVisibilityOffset off
                      $ addCursorOffset off
                      $ result & image .~ paddedImage

center :: Widget -> Widget
center = centerWith Nothing

centerWith :: Maybe Char -> Widget -> Widget
centerWith c = vCenterWith c . hCenterWith c

centerAbout :: Location -> Widget -> Widget
centerAbout loc p =
    Widget Unlimited Unlimited $ do
      -- Compute translation offset so that loc is in the middle of the
      -- rendering area
      c <- getContext
      let centerW = c^.availW `div` 2
          centerH = c^.availH `div` 2
          off = Location ( centerW - loc^.column
                         , centerH - loc^.row
                         )
      render $ translateBy off p
