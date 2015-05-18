module Brick.Center
  ( hCentered
  , hCenteredWith
  , vCentered
  , vCenteredWith
  , centered
  )
where

import Brick.Prim (Prim(..), Priority(..))

hCentered :: Prim -> Prim
hCentered = hCenteredWith ' '

hCenteredWith :: Char -> Prim -> Prim
hCenteredWith c p =
    HBox [ (HPad c, Low)
         , (p, High)
         , (HPad c, Low)
         ]

vCentered :: Prim -> Prim
vCentered = vCenteredWith ' '

vCenteredWith :: Char -> Prim -> Prim
vCenteredWith c p =
    VBox [ (VPad c, Low)
         , (p, High)
         , (VPad c, Low)
         ]

centered :: Prim -> Prim
centered = vCentered . hCentered
