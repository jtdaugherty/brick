{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.IMap
    ( IMap
    , Run(..)
    , empty
    , Data.IMap.null
    , singleton
    , insert
    , delete
    , restrict
    , lookup
    , splitLE
    , intersectionWith
    , mapMaybe
    , addToKeys
    , unsafeUnion
    , fromList
    , unsafeRuns
    , unsafeToAscList
    ) where

import Data.List (foldl')
import Data.Monoid
import Data.IntMap.Strict (IntMap)
import GHC.Generics
import Control.DeepSeq
import Prelude hiding (lookup)
import qualified Data.IntMap.Strict as IM

-- | Semantically, 'IMap' and 'IntMap' are identical; but 'IMap' is more
-- efficient when large sequences of contiguous keys are mapped to the same
-- value.
newtype IMap a = IMap { _runs :: IntMap (Run a) } deriving (Show, Functor, Read, Generic, NFData)

{-# INLINE unsafeRuns #-}
-- | This function is unsafe because 'IMap's that compare equal may split their
-- runs into different chunks; consumers must promise that they do not treat
-- run boundaries specially.
unsafeRuns :: IMap a -> IntMap (Run a)
unsafeRuns = _runs

instance Eq a => Eq (IMap a) where
    IMap m == IMap m' = go (IM.toAscList m) (IM.toAscList m') where
        go ((k, Run n a):kvs) ((k', Run n' a'):kvs')
            = k == k' && a == a' && case compare n n' of
                LT -> go kvs ((k'+n, Run (n'-n) a'):kvs')
                EQ -> go kvs kvs'
                GT -> go ((k+n', Run (n-n') a):kvs) kvs'
        go [] [] = True
        go _ _ = False

instance Ord a => Ord (IMap a) where
    compare (IMap m) (IMap m') = go (IM.toAscList m) (IM.toAscList m') where
        go [] [] = EQ
        go [] _  = LT
        go _  [] = GT
        go ((k, Run n a):kvs) ((k', Run n' a'):kvs')
            = compare k k' <> compare a a' <> case compare n n' of
                LT -> go kvs ((k'+n, Run (n'-n) a'):kvs')
                EQ -> go kvs kvs'
                GT -> go ((k+n', Run (n-n') a):kvs) kvs'

-- | Zippy: '(<*>)' combines values at equal keys, discarding any values whose
-- key is in only one of its two arguments.
instance Applicative IMap where
    pure a = IMap . IM.fromDistinctAscList $
        [ (minBound, Run maxBound a)
        , (-1, Run maxBound a)
        , (maxBound-1, Run 2 a)
        ]
    (<*>) = intersectionWith ($)

-- | @Run n a@ represents @n@ copies of the value @a@.
data Run a = Run
    { len :: !Int
    , val :: !a
    } deriving (Eq, Ord, Read, Show, Functor, Generic, NFData)

instance Foldable    Run where foldMap f r = f (val r)
instance Traversable Run where sequenceA (Run n v) = Run n <$> v

empty :: IMap a
empty = IMap IM.empty

null :: IMap a -> Bool
null = IM.null . _runs

singleton :: Int -> Run a -> IMap a
singleton k r
    | len r >= 1 = IMap (IM.singleton k r)
    | otherwise = empty

insert :: Int -> Run a -> IMap a -> IMap a
insert k r m
    | len r < 1 = m
    | otherwise = m { _runs = IM.insert k r (_runs (delete k r m)) }

{-# INLINE delete #-}
delete :: Int -> Run ignored -> IMap a -> IMap a
delete k r m
    | len r < 1 = m
    | otherwise = m { _runs = IM.union (_runs lt) (_runs gt) }
    where
    (lt, ge) = splitLE (k-1) m
    (_ , gt) = splitLE (k+len r-1) ge

-- | Given a range of keys (as specified by a starting key and a length for
-- consistency with other functions in this module), restrict the map to keys
-- in that range. @restrict k r m@ is equivalent to @intersectionWith const m
-- (insert k r empty)@ but potentially more efficient.
restrict :: Int -> Run ignored -> IMap a -> IMap a
restrict k r = id
    . snd
    . splitLE (k-1)
    . fst
    . splitLE (k+len r-1)

lookup :: Int -> IMap a -> Maybe a
lookup k m = case IM.lookupLE k (_runs m) of
    Just (k', Run n a) | k < k'+n -> Just a
    _ -> Nothing

-- | @splitLE n m@ produces a tuple @(le, gt)@ where @le@ has all the
-- associations of @m@ where the keys are @<= n@ and @gt@ has all the
-- associations of @m@ where the keys are @> n@.
splitLE :: Int -> IMap a -> (IMap a, IMap a)
splitLE k m = case IM.lookupLE k (_runs m) of
    Nothing -> (empty, m)
    Just (k', r@(Run n _)) -> case (k' + n - 1 <= k, k' == k) of
        (True , False) -> (m { _runs = lt }, m { _runs = gt })
        (True , True ) -> (m { _runs = IM.insert k r lt }, m { _runs = gt })
        (False, _    ) -> ( m { _runs = IM.insert k'    r { len =     1 + k - k' } lt' }
                          , m { _runs = IM.insert (k+1) r { len = n - 1 - k + k' } gt' }
                          )
        where
        (lt', gt') = IM.split k' (_runs m)
    where
    (lt, gt) = IM.split k (_runs m)

-- | Increment all keys by the given amount. This is like
-- 'IM.mapKeysMonotonic', but restricted to partially-applied addition.
addToKeys :: Int -> IMap a -> IMap a
addToKeys n m = m { _runs = IM.mapKeysMonotonic (n+) (_runs m) }

-- TODO: This is pretty inefficient. IntMap offers some splitting functions
-- that should make it possible to be more efficient here (though the
-- implementation would be significantly messier).
intersectionWith :: (a -> b -> c) -> IMap a -> IMap b -> IMap c
intersectionWith f (IMap runsa) (IMap runsb)
    = IMap . IM.fromDistinctAscList $ merge (IM.toAscList runsa) (IM.toAscList runsb)
    where
    merge as@((ka, ra):at) bs@((kb, rb):bt)
        | ka' < kb = merge at bs
        | kb' < ka = merge as bt
        | otherwise = (kc, Run (kc' - kc + 1) vc) : case compare ka' kb' of
            LT -> merge at bs
            EQ -> merge at bt
            GT -> merge as bt
        where
        ka' = ka + len ra - 1
        kb' = kb + len rb - 1
        kc  = max ka  kb
        kc' = min ka' kb'
        vc  = f (val ra) (val rb)
    merge _ _ = []

mapMaybe :: (a -> Maybe b) -> IMap a -> IMap b
mapMaybe f (IMap runs) = IMap (IM.mapMaybe (traverse f) runs)

fromList :: [(Int, Run a)] -> IMap a
fromList = foldl' (\m (k, r) -> insert k r m) empty

-- | This function is unsafe because 'IMap's that compare equal may split their
-- runs into different chunks; consumers must promise that they do not treat
-- run boundaries specially.
unsafeToAscList :: IMap a -> [(Int, Run a)]
unsafeToAscList = IM.toAscList . _runs

-- | This function is unsafe because it assumes there is no overlap between its
-- arguments. That is, in the call @unsafeUnion a b@, the caller must guarantee
-- that if @lookup k a = Just v@ then @lookup k b = Nothing@ and vice versa.
unsafeUnion :: IMap a -> IMap a -> IMap a
unsafeUnion a b = IMap { _runs = _runs a `IM.union` _runs b }
