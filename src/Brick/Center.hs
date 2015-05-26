module Brick.Center
  ( hCenter
  , hCenterWith
  , vCenter
  , vCenterWith
  , center
  )
where

import Brick.Prim

hCenter :: Prim a -> Prim a
hCenter = hCenterWith ' '

hCenterWith :: Char -> Prim a -> Prim a
hCenterWith c p =
    hBox [ (hPad c, Low)
         , (p, High)
         , (hPad c, Low)
         ]

vCenter :: Prim a -> Prim a
vCenter = vCenterWith ' '

vCenterWith :: Char -> Prim a -> Prim a
vCenterWith c p =
    vBox [ (vPad c, Low)
         , (p, High)
         , (vPad c, Low)
         ]

center :: Prim a -> Prim a
center = vCenter . hCenter
