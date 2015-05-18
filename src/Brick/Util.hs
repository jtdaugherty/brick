module Brick.Util
  ( for
  , clamp
  , on
  , fg
  , bg
  , clOffset
  )
where

import Data.Monoid ((<>))
import Graphics.Vty

import Brick.Core (Location(..), CursorLocation(..))

for :: [a] -> (a -> b) -> [b]
for = flip map

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx val = max mn (min val mx)

on :: Color -> Color -> Attr
on f b = defAttr `withForeColor` f
                 `withBackColor` b

fg :: Color -> Attr
fg = (defAttr `withForeColor`)

bg :: Color -> Attr
bg = (defAttr `withBackColor`)

clOffset :: CursorLocation -> Location -> CursorLocation
clOffset cl loc = cl { cursorLocation = (cursorLocation cl) <> loc }
