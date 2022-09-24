{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Brick.BorderMap
    ( BorderMap
    , Edges(..)
    , eTopL, eBottomL, eRightL, eLeftL
    , empty, clear, emptyCoordinates, singleton
    , insertH, insertV, insert
    , unsafeUnion
    , coordinates, bounds
    , values
    , lookupRow, lookupCol, lookupH, lookupV, lookup
    , setCoordinates, crop, expand
    , translate
    ) where

import Brick.Types.Common (Edges(..), Location(..), eTopL, eBottomL, eRightL, eLeftL, origin)
import Control.Applicative (liftA2)
import Data.IMap (IMap, Run(Run))
import GHC.Generics
import Control.DeepSeq
import Prelude hiding (lookup)
import qualified Data.IMap as IM

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
    } deriving (Eq, Ord, Show, Functor, Read, Generic, NFData)

-- | Given a rectangle (specified as the coordinates of the top, left, bottom,
-- and right sides), initialize an empty 'BorderMap'.
emptyCoordinates :: Edges Int -> BorderMap a
emptyCoordinates cs = BorderMap { _coordinates = cs, _values = pure IM.empty }

-- | An empty 'BorderMap' that tracks the same points as the input.
clear :: BorderMap a -> BorderMap b
clear = emptyCoordinates . coordinates

-- | An empty 'BorderMap' that does not track any points.
empty :: BorderMap a
empty = emptyCoordinates Edges
    { eTop = 0
    , eBottom = -1
    , eLeft = 0
    , eRight = -1
    }

-- | A 'BorderMap' that tracks only the given the point (and initially maps it
-- to the given value).
singleton :: Location -> a -> BorderMap a
singleton l v = translate l . insert origin v . emptyCoordinates $ pure 0

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

-- | Insert a single value at the given location.
insert :: Location -> a -> BorderMap a -> BorderMap a
insert l = insertV l . Run 1

-- | Look up all values on a given row. The 'IMap' returned maps columns to
-- values.
lookupRow :: Int -> BorderMap a -> IMap a
lookupRow row m
    | row == eTop    (coordinates m) = eTop    (_values m)
    | row == eBottom (coordinates m) = eBottom (_values m)
    | otherwise = IM.fromList
        $  [(eLeft   (coordinates m), Run 1 a) | Just a <- [IM.lookup row (eLeft   (_values m))]]
        ++ [(eRight  (coordinates m), Run 1 a) | Just a <- [IM.lookup row (eRight  (_values m))]]

-- | Look up all values on a given column. The 'IMap' returned maps rows to
-- values.
lookupCol :: Int -> BorderMap a -> IMap a
lookupCol col m
    | col == eLeft   (coordinates m) = eLeft   (_values m)
    | col == eRight  (coordinates m) = eRight  (_values m)
    | otherwise = IM.fromList
        $  [(eTop    (coordinates m), Run 1 a) | Just a <- [IM.lookup col (eTop    (_values m))]]
        ++ [(eBottom (coordinates m), Run 1 a) | Just a <- [IM.lookup col (eBottom (_values m))]]

-- | Bulk lookup of horizontally-adjacent values. The 'Location' gives the
-- starting point, and the 'Run' extends in the "larger columns" direction. The
-- 'IMap' returned maps columns to values.
lookupH :: Location -> Run ignored -> BorderMap a -> IMap a
lookupH (Location (col, row)) r = IM.restrict col r . lookupRow row

-- | Bulk lookup of vertically-adjacent values. The 'Location' gives the
-- starting point, and the 'Run' extends in the "larger rows" direction. The
-- 'IMap' returned maps rows to values.
lookupV :: Location -> Run ignored -> BorderMap a -> IMap a
lookupV (Location (col, row)) r = IM.restrict row r . lookupCol col

-- | Look up a single position.
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
    values' = gc
        <$> _coordinates m
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
