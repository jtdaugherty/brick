module Brick.Center
  ( hCenter
  , hCenterWith
  , vCenter
  , vCenterWith
  , center
  )
where

import Brick.Render

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
