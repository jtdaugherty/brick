-- | Utility functions.
module Brick.Util
  ( clamp
  , on
  , fg
  , bg
  , clOffset
  )
where

import Control.Lens ((&), (%~))
import Data.Monoid ((<>))
import Graphics.Vty

import Brick.Types (Location(..), CursorLocation(..), cursorLocationL)

-- | Given a minimum value and a maximum value, clamp a value to that
-- range (values less than the minimum map to the minimum and values
-- greater than the maximum map to the maximum).
--
-- >>> clamp 1 10 11
-- 10
-- >>> clamp 1 10 2
-- 2
-- >>> clamp 5 10 1
-- 5
clamp :: (Ord a)
      => a
      -- ^ The minimum value
      -> a
      -- ^ The maximum value
      -> a
      -- ^ The value to clamp
      -> a
clamp mn mx val = max mn (min val mx)

-- | Build an attribute from a foreground color and a background color.
-- Intended to be used infix.
on :: Color
   -- ^ The foreground color
   -> Color
   -- ^ The background color
   -> Attr
on f b = defAttr `withForeColor` f
                 `withBackColor` b

-- | Create an attribute from the specified foreground color (the
-- background color is the "default").
fg :: Color -> Attr
fg = (defAttr `withForeColor`)

-- | Create an attribute from the specified background color (the
-- background color is the "default").
bg :: Color -> Attr
bg = (defAttr `withBackColor`)

-- | Add a 'Location' offset to the specified 'CursorLocation'.
clOffset :: CursorLocation -> Location -> CursorLocation
clOffset cl off = cl & cursorLocationL %~ (<> off)
