module Brick.Center
  ( hCentered
  , hCenteredWith
  , vCentered
  , vCenteredWith
  , centered
  )
where

import Brick.Prim (Prim(..), Priority(..))

hCentered :: Prim a -> Prim a
hCentered = hCenteredWith ' '

hCenteredWith :: Char -> Prim a -> Prim a
hCenteredWith c p =
    HBox [ (HPad c, Low)
         , (p, High)
         , (HPad c, Low)
         ]

vCentered :: Prim a -> Prim a
vCentered = vCenteredWith ' '

vCenteredWith :: Char -> Prim a -> Prim a
vCenteredWith c p =
    VBox [ (VPad c, Low)
         , (p, High)
         , (VPad c, Low)
         ]

centered :: Prim a -> Prim a
centered = vCentered . hCentered
