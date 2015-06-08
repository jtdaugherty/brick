module Brick.Center
  ( hCenter
  , hCenterWith
  , vCenter
  , vCenterWith
  , center
  , centerAbout
  )
where

import Control.Lens ((^.))
import Control.Monad.Trans.Reader

import Brick.Render
import Brick.Core

hCenter :: Render -> Render
hCenter = hCenterWith ' '

hCenterWith :: Char -> Render -> Render
hCenterWith c p =
    hBox [ (hPad c, Low)
         , (p, High)
         , (hPad c, Low)
         ]

vCenter :: Render -> Render
vCenter = vCenterWith ' '

vCenterWith :: Char -> Render -> Render
vCenterWith c p =
    vBox [ (vPad c, Low)
         , (p, High)
         , (vPad c, Low)
         ]

center :: Render -> Render
center = vCenter . hCenter

centerAbout :: Location -> Render -> Render
centerAbout (Location (offW, offH)) p = do
    -- Compute translation offset so that loc is in the middle of the
    -- rendering area
    c <- ask
    let centerW = c^.w `div` 2
        centerH = c^.h `div` 2
        off = Location ( centerW - offW
                       , centerH - offH
                       )
    translate off p
