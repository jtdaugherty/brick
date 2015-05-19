module Brick.Center
  ( hCenter
  , hCenterWith
  , vCenter
  , vCenterWith
  , center
  )
where

import Brick.Prim (Prim(..), Priority(..))

hCenter :: Prim a -> Prim a
hCenter = hCenterWith ' '

hCenterWith :: Char -> Prim a -> Prim a
hCenterWith c p =
    HBox [ (HPad c, Low)
         , (p, High)
         , (HPad c, Low)
         ]

vCenter :: Prim a -> Prim a
vCenter = vCenterWith ' '

vCenterWith :: Char -> Prim a -> Prim a
vCenterWith c p =
    VBox [ (VPad c, Low)
         , (p, High)
         , (VPad c, Low)
         ]

center :: Prim a -> Prim a
center = vCenter . hCenter
