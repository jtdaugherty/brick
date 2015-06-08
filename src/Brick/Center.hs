module Brick.Center
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

import Brick.Render
import Brick.Core

hCenter :: Render -> Render
hCenter = hCenterWith Nothing

hCenterWith :: Maybe Char -> Render -> Render
hCenterWith Nothing p = do
    result <- p
    c <- getContext
    let offW = (c^.availW - (result^.image.to imageWidth)) `div` 2
    translateBy (Location (offW, 0)) $ return result
hCenterWith (Just c) p =
    hBox [ (hPad c, Low)
         , (p, High)
         , (hPad c, Low)
         ]

vCenter :: Render -> Render
vCenter = vCenterWith Nothing

vCenterWith :: (Maybe Char) -> Render -> Render
vCenterWith Nothing p = do
    result <- p
    c <- getContext
    let offH = (c^.availH - (result^.image.to imageHeight)) `div` 2
    translateBy (Location (0, offH)) $ return result
vCenterWith (Just c) p =
    vBox [ (vPad c, Low)
         , (p, High)
         , (vPad c, Low)
         ]

center :: Render -> Render
center = centerWith Nothing

centerWith :: Maybe Char -> Render -> Render
centerWith c = vCenterWith c . hCenterWith c

centerAbout :: Location -> Render -> Render
centerAbout (Location (offW, offH)) p = do
    -- Compute translation offset so that loc is in the middle of the
    -- rendering area
    c <- getContext
    let centerW = c^.availW `div` 2
        centerH = c^.availH `div` 2
        off = Location ( centerW - offW
                       , centerH - offH
                       )
    translateBy off p
