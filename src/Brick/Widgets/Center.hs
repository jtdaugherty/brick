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

import Control.Lens ((^.), to)
import Graphics.Vty (imageWidth, imageHeight)

import Brick.Widgets.Core
import Brick.Core

hCenter :: Widget -> Widget
hCenter = hCenterWith Nothing

hCenterWith :: Maybe Char -> Widget -> Widget
hCenterWith Nothing p =
    Widget Unlimited (vSize p) $ do
      result <- render p
      c <- getContext
      let offW = (c^.availW - (result^.image.to imageWidth)) `div` 2
      render $ translateBy (Location (offW, 0)) $ Widget Fixed Fixed $ return result
hCenterWith (Just c) p =
    hBox [ (hPad c, Low)
         , (p, High)
         , (hPad c, Low)
         ]

vCenter :: Widget -> Widget
vCenter = vCenterWith Nothing

vCenterWith :: (Maybe Char) -> Widget -> Widget
vCenterWith Nothing p =
    Widget (hSize p) Unlimited $ do
      result <- render p
      c <- getContext
      let offH = (c^.availH - (result^.image.to imageHeight)) `div` 2
      render $ translateBy (Location (0, offH)) $ Widget Fixed Fixed $ return result
vCenterWith (Just c) p =
    vBox [ (vPad c, Low)
         , (p, High)
         , (vPad c, Low)
         ]

center :: Widget -> Widget
center = centerWith Nothing

centerWith :: Maybe Char -> Widget -> Widget
centerWith c = vCenterWith c . hCenterWith c

centerAbout :: Location -> Widget -> Widget
centerAbout (Location (offW, offH)) p =
    Widget Unlimited Unlimited $ do
      -- Compute translation offset so that loc is in the middle of the
      -- rendering area
      c <- getContext
      let centerW = c^.availW `div` 2
          centerH = c^.availH `div` 2
          off = Location ( centerW - offW
                         , centerH - offH
                         )
      render $ translateBy off p
