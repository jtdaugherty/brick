module Brick.Center
  ( hCenter
  , hCenterWith
  , vCenter
  , vCenterWith
  , center
  )
where

import Brick.Prim

hCenter :: Render a -> Render a
hCenter = hCenterWith ' '

hCenterWith :: Char -> Render a -> Render a
hCenterWith c p =
    hBox [ (hPad c, Low)
         , (p, High)
         , (hPad c, Low)
         ]

vCenter :: Render a -> Render a
vCenter = vCenterWith ' '

vCenterWith :: Char -> Render a -> Render a
vCenterWith c p =
    vBox [ (vPad c, Low)
         , (p, High)
         , (vPad c, Low)
         ]

center :: Render a -> Render a
center = vCenter . hCenter
