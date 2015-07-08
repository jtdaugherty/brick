-- | Basic types used by this library.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Brick.Types
  ( Location(Location)
  , TerminalLocation(..)
  , CursorLocation(..)
  , cursorLocation
  , cursorLocationName
  , HandleEvent(..)
  , Name(..)
  , suffixLenses
  )
where

import Control.Lens
import Data.String
import Data.Monoid (Monoid(..))
import Graphics.Vty (Event)
import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

-- | A terminal screen location.
data Location = Location { _loc :: (Int, Int)
                         -- ^ (Column, Row)
                         }
                deriving Show

makeLenses ''Location

instance Field1 Location Location Int Int where
    _1 = loc._1

instance Field2 Location Location Int Int where
    _2 = loc._2

-- | The class of types that behave like terminal locations.
class TerminalLocation a where
    -- | Get the column out of the value
    column :: Lens' a Int
    -- | Get the row out of the value
    row :: Lens' a Int

instance TerminalLocation Location where
    column = _1
    row = _2

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
    CursorLocation { _cursorLocation :: !Location
                   -- ^ The location
                   , _cursorLocationName :: !(Maybe Name)
                   -- ^ The name of the widget associated with the location
                   }
                   deriving Show

makeLenses ''CursorLocation

instance TerminalLocation CursorLocation where
    column = cursorLocation._1
    row = cursorLocation._2

class HandleEvent a where
    handleEvent :: Event -> a -> a

-- | A template haskell function to build lenses for a record type. This
-- function differs from the 'Lens.makeLenses' function in that it does
-- not require the record fields to be prefixed with underscores and
-- it adds an "L" suffix to lens names to make it clear that they are
-- lenses.
suffixLenses :: TH.Name -> TH.DecsQ
suffixLenses = makeLensesWith $
  lensRules & lensField .~ (\_ _ name -> [TopName $ TH.mkName $ TH.nameBase name ++ "L"])
