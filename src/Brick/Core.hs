{-# LANGUAGE TemplateHaskell #-}
module Brick.Core
  ( Location(Location)
  , loc
  , CursorName(..)
  , CursorLocation(..)
  , HandleEvent(..)
  )
where

import Control.Lens
import Data.Monoid (Monoid(..))
import Graphics.Vty (Event, DisplayRegion)

data Location = Location { _loc :: (Int, Int)
                         }
                deriving Show

makeLenses ''Location

origin :: Location
origin = Location (0, 0)

instance Monoid Location where
    mempty = origin
    mappend (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

newtype CursorName = CursorName String
                     deriving (Eq, Show)

data CursorLocation =
    CursorLocation { cursorLocation :: !Location
                   , cursorLocationName :: !(Maybe CursorName)
                   }
                   deriving Show

class HandleEvent a where
    handleEvent :: Event -> a -> a
