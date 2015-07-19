-- | Basic types used by this library.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Brick.Types
  ( Location(..)
  , locL
  , TerminalLocation(..)
  , CursorLocation(..)
  , cursorLocationL
  , cursorLocationNameL
  , HandleEvent(..)
  , Name(..)
  , suffixLenses
  )
where

import Control.Lens
import Data.String
import Data.Monoid (Monoid(..))
import Graphics.Vty (Event)

import Brick.Types.TH

-- | A terminal screen location.
data Location = Location { loc :: (Int, Int)
                         -- ^ (Column, Row)
                         }
                deriving Show

suffixLenses ''Location

instance Field1 Location Location Int Int where
    _1 = locL._1

instance Field2 Location Location Int Int where
    _2 = locL._2

-- | The class of types that behave like terminal locations.
class TerminalLocation a where
    -- | Get the column out of the value
    columnL :: Lens' a Int
    column :: a -> Int
    -- | Get the row out of the value
    rowL :: Lens' a Int
    row :: a -> Int

instance TerminalLocation Location where
    columnL = _1
    column (Location t) = fst t
    rowL = _2
    row (Location t) = snd t

-- | Names of things. Used to name cursor locations, widgets, and
-- viewports.
newtype Name = Name String
             deriving (Eq, Show, Ord)

instance IsString Name where
    fromString = Name

-- | The origin (upper-left corner).
origin :: Location
origin = Location (0, 0)

instance Monoid Location where
    mempty = origin
    mappend (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

-- | A cursor location.  These are returned by the rendering process.
data CursorLocation =
    CursorLocation { cursorLocation :: !Location
                   -- ^ The location
                   , cursorLocationName :: !(Maybe Name)
                   -- ^ The name of the widget associated with the location
                   }
                   deriving Show

suffixLenses ''CursorLocation

instance TerminalLocation CursorLocation where
    columnL = cursorLocationL._1
    column = column . cursorLocation
    rowL = cursorLocationL._2
    row = row . cursorLocation

-- | The class of types that provide some basic event-handling.
class HandleEvent a where
    -- | Handle a Vty event
    handleEvent :: Event -> a -> a
