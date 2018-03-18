{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.BorderMap
    ( BorderMap
    , Location(..), origin, locL
    , Edges(..)
    , eTopL, eBottomL, eRightL, eLeftL
    , empty, emptyCoordinates, singleton
    , insertH, insertV, insert
    , unsafeUnion
    , coordinates, bounds
    , values
    , lookupRow, lookupCol, lookupH, lookupV, lookup
    , setCoordinates, crop, expand
    , translate
    ) where

import Brick.Types.TH (suffixLenses)
import Control.Applicative (liftA2)
import qualified Data.Semigroup as Sem
import Data.IMap (IMap, Run(Run))
import Lens.Micro (_1, _2)
import Lens.Micro.Internal (Field1, Field2)
import Prelude hiding (lookup)
import qualified Data.IMap as IM

-- | A terminal screen location.
data Location = Location { loc :: (Int, Int)
                         -- ^ (Column, Row)
                         }
                deriving (Show, Eq, Ord)

suffixLenses ''Location

instance Field1 Location Location Int Int where
    _1 = locL._1

instance Field2 Location Location Int Int where
    _2 = locL._2

-- | The origin (upper-left corner).
origin :: Location
origin = Location (0, 0)

instance Sem.Semigroup Location where
    (Location (w1, h1)) <> (Location (w2, h2)) = Location (w1+w2, h1+h2)

instance Monoid Location where
    mempty = origin
    mappend = (Sem.<>)

data Edges a = Edges { eTop, eBottom, eLeft, eRight :: a }
    deriving (Eq, Ord, Read, Show, Functor)

suffixLenses ''Edges

instance Applicative Edges where
    pure a = Edges a a a a
    Edges ft fb fl fr <*> Edges vt vb vl vr =
        Edges (ft vt) (fb vb) (fl vl) (fr vr)

instance Monad Edges where
    Edges vt vb vl vr >>= f = Edges
        (eTop    (f vt))
        (eBottom (f vb))
        (eLeft   (f vl))
        (eRight  (f vr))

-- | Internal use only.
neighbors :: Edges a -> Edges (a, a)
neighbors (Edges vt vb vl vr) = Edges horiz horiz vert vert where
    horiz = (vl, vr)
    vert  = (vt, vb)

-- Invariant: corner values are present on all the edges incident on that
-- corner. Widthless or heightless rectangles replicate the IMaps exactly on
-- the two coincident edges.
--
-- Practically speaking, this means for lookup you can look on any edge that
-- could contain the key you care about, while for insertion you must insert on
-- every edge that could contain the keys being inserted.

-- | A @BorderMap a@ is like a @Map Location a@, except that there is a
-- rectangle, and only 'Location's on the border of this rectangle are
-- retained. The 'BorderMap' can be queried for the position and size of the
-- rectangle. There are also efficient bulk query and bulk update operations
-- for adjacent positions on the border.
data BorderMap a = BorderMap
    { _coordinates :: Edges Int
    , _values :: Edges (IMap a)
    } deriving (Eq, Ord, Show, Functor)

-- | Given a rectangle (specified as the coordinates of the top, left, bottom,
-- and right sides), initialize an empty 'BorderMap'.
emptyCoordinates :: Edges Int -> BorderMap a
emptyCoordinates cs = BorderMap { _coordinates = cs, _values = pure IM.empty }

-- | An empty 'BorderMap' that only tracks the point (0,0).
empty :: BorderMap a
empty = emptyCoordinates (pure 0)

-- | A 'BorderMap' that tracks only the given the point (and initially maps it
-- to the given value).
singleton :: Location -> a -> BorderMap a
singleton l v = translate l . insert origin v $ empty

{-# INLINE coordinates #-}
-- | The positions of the edges of the rectangle whose border is retained in a
-- 'BorderMap'. For example, if @coordinates m = e@, then the top border
-- contains the 'Location's on row @eTop e@ and between columns @eLeft e@ to
-- @eRight e@ inclusive.
coordinates :: BorderMap a -> Edges Int
coordinates = _coordinates

-- | A complementary way to query the edges of the rectangle whose border is
-- retained in a 'BorderMap'. For example, if @bounds m = b@, then a
-- 'Location'\'s column must be between @fst (eTop b)@ and @snd (eTop b)@ to be
-- retained. See also 'coordinates', which is in most cases a more natural
-- border query.
bounds :: BorderMap a -> Edges (Int, Int)
bounds = neighbors . coordinates

{-# INLINE values #-}
-- | Maps giving the values along each edge. Corner values are replicated in
-- all relevant edges.
values :: BorderMap a -> Edges (IMap a)
values = _values

-- | Bulk insertion of horizontally-adjacent values. The 'Location' gives the
-- start point, and the 'Run' extends in the "larger columns" direction.
insertH :: Location -> Run a -> BorderMap a -> BorderMap a
insertH = insertDirAgnostic (Edges insertPar insertPar insertPerp insertPerp) . swapLoc
    where
    swapLoc (Location (col, row)) = Location (row, col)

-- | Bulk insertion of vertically-adjacent values. The 'Location' gives the
-- start point, and the 'Run' extends in the "larger rows" direction.
insertV :: Location -> Run a -> BorderMap a -> BorderMap a
insertV = insertDirAgnostic (Edges insertPerp insertPerp insertPar insertPar)

insertDirAgnostic
    :: Edges (Location -> Run a -> Int -> (Int, Int) -> IMap a -> IMap a)
    -> Location -> Run a -> BorderMap a -> BorderMap a
insertDirAgnostic insertions l r m =
    m { _values = insertions <*> pure l <*> pure r <*> coordinates m <*> bounds m <*> _values m }

insertPar, insertPerp :: Location -> Run a -> Int -> (Int, Int) -> IMap a -> IMap a
insertPar (Location (kPar, kPerp)) r herePar (loPerp, hiPerp)
    | kPar == herePar && loPerp <= kPerp + IM.len r - 1 && kPerp <= hiPerp
        = IM.insert beg r { IM.len = end - beg + 1 }
    | otherwise = id
    where
    beg = max kPerp loPerp
    end = min (kPerp + IM.len r - 1) hiPerp
insertPerp (Location (kPar, kPerp)) r herePerp (loPar, hiPar)
    | loPar <= kPar && kPar <= hiPar && kPerp <= herePerp && herePerp <= kPerp + IM.len r - 1
        = IM.insert kPar r { IM.len = 1 }
    | otherwise = id

insert :: Location -> a -> BorderMap a -> BorderMap a
insert l = insertV l . Run 1

lookupRow :: Int -> BorderMap a -> IMap a
lookupRow row m
    | row == eTop    (coordinates m) = eTop    (_values m)
    | row == eBottom (coordinates m) = eBottom (_values m)
    | otherwise = IM.fromList
        $  [(eLeft   (coordinates m), Run 1 a) | Just a <- [IM.lookup row (eLeft   (_values m))]]
        ++ [(eRight  (coordinates m), Run 1 a) | Just a <- [IM.lookup row (eRight  (_values m))]]

lookupCol :: Int -> BorderMap a -> IMap a
lookupCol col m
    | col == eLeft   (coordinates m) = eLeft   (_values m)
    | col == eRight  (coordinates m) = eRight  (_values m)
    | otherwise = IM.fromList
        $  [(eTop    (coordinates m), Run 1 a) | Just a <- [IM.lookup col (eTop    (_values m))]]
        ++ [(eBottom (coordinates m), Run 1 a) | Just a <- [IM.lookup col (eBottom (_values m))]]

lookupH :: Location -> Run ignored -> BorderMap a -> IMap a
lookupH (Location (col, row)) r = IM.restrict col r . lookupRow row

lookupV :: Location -> Run ignored -> BorderMap a -> IMap a
lookupV (Location (col, row)) r = IM.restrict row r . lookupCol col

lookup :: Location -> BorderMap a -> Maybe a
lookup (Location (col, row)) = IM.lookup row . lookupCol col

-- | Set the rectangle being tracked by this 'BorderMap', throwing away any
-- values that do not lie on this new rectangle.
setCoordinates :: Edges Int -> BorderMap a -> BorderMap a
setCoordinates coordinates' m = BorderMap
    { _values = values'
    , _coordinates = coordinates'
    }
    where
    bounds' = neighbors coordinates'
    values' = pure gc
        <*> _coordinates m
        <*> coordinates'
        <*> bounds'
        <*> _values m
        <*> Edges { eTop = lookupRow, eBottom = lookupRow, eLeft = lookupCol, eRight = lookupCol }
    gc oldPar newPar (loPerp, hiPerp) imPar lookupPerp
        | oldPar == newPar = IM.restrict loPerp (Run (hiPerp-loPerp+1) ()) imPar
        | otherwise = lookupPerp newPar m

-- | Ensure that the rectangle being tracked by this 'BorderMap' extends no
-- farther than the given one.
crop :: Edges Int -> BorderMap a -> BorderMap a
crop cs m = setCoordinates (shrink <*> cs <*> coordinates m) m where
    shrink = Edges
        { eTop    = max
        , eBottom = min
        , eLeft   = max
        , eRight  = min
        }

-- | Ensure that the rectangle being tracked by this 'BorderMap' extends at
-- least as far as the given one.
expand :: Edges Int -> BorderMap a -> BorderMap a
expand cs m = setCoordinates (grow <*> cs <*> coordinates m) m where
    grow = Edges
        { eTop    = min
        , eBottom = max
        , eLeft   = min
        , eRight  = max
        }

-- | Move a 'BorderMap' by adding the given 'Location' to all keys in the map.
translate :: Location -> BorderMap a -> BorderMap a
-- fast path: do nothing for (0,0)
translate (Location (0, 0)) m = m
translate (Location (c, r)) m = BorderMap
    { _coordinates = liftA2 (+)          cOffsets (_coordinates m)
    , _values      = liftA2 IM.addToKeys vOffsets (_values      m)
    }
    where
    cOffsets = Edges { eTop = r, eBottom = r, eLeft = c, eRight = c }
    vOffsets = Edges { eTop = c, eBottom = c, eLeft = r, eRight = r }

-- | Assumes the two 'BorderMap's are tracking the same rectangles, but have
-- disjoint keys. This property is not checked.
unsafeUnion :: BorderMap a -> BorderMap a -> BorderMap a
unsafeUnion m m' = m { _values = liftA2 IM.unsafeUnion (_values m) (_values m') }
