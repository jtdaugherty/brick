{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

import Control.Applicative
import Data.Bool (bool)
import Data.Traversable (sequenceA)
import System.Exit (exitFailure, exitSuccess)

import Data.IMap (IMap, Run(Run))
import Data.IntMap (IntMap)
import Test.QuickCheck
import qualified Data.IMap as IMap
import qualified Data.IntMap as IntMap

import qualified List
import qualified Render

instance Arbitrary v => Arbitrary (Run v) where
    arbitrary = liftA2 (\(Positive n) -> Run n) arbitrary arbitrary

instance Arbitrary v => Arbitrary (IMap v) where
    arbitrary = IMap.fromList <$> arbitrary

instance (a ~ Ordering, Show b) => Show (a -> b) where
    show f = show [f x | x <- [minBound .. maxBound]]

lower :: IMap v -> IntMap v
lower m = IntMap.fromDistinctAscList
    [ (base+offset, v)
    | (base, Run n v) <- IMap.unsafeToAscList m
    , offset <- [0..n-1]
    ]

raise :: Eq v => IntMap v -> IMap v
raise = IMap.fromList . rle . map singletonRun . IntMap.toAscList where
    singletonRun (k, v) = (k, Run 1 v)

    rle ((k, Run n v):(k', Run n' v'):kvs)
        | k+n == k' && v == v' = rle ((k, Run (n+n') v):kvs)
    rle (kv:kvs) = kv:rle kvs
    rle [] = []

lowerRun :: Int -> Run v -> IntMap v
lowerRun k r = IntMap.fromAscList [(k+offset, IMap.val r) | offset <- [0..IMap.len r-1]]

type O = Ordering
type I = IMap Ordering

-- These next two probably have overflow bugs that QuickCheck can't reasonably
-- notice. Hopefully they don't come up in real use cases...
prop_raiseLowerFaithful :: IntMap O -> Bool
prop_raiseLowerFaithful m = m == lower (raise m)

prop_equalityReflexive :: I -> Bool
prop_equalityReflexive m = m == raise (lower m)

prop_equality :: I -> I -> Bool
prop_equality l r = (l == r) == (lower l == lower r)

prop_compare :: I -> I -> Bool
prop_compare l r = compare l r == compare (lower l) (lower r)

prop_applicativeIdentity :: I -> Bool
prop_applicativeIdentity v = (id <$> v) == v

prop_applicativeComposition :: IMap (O -> O) -> IMap (O -> O) -> IMap O -> Bool
prop_applicativeComposition u v w = ((.) <$> u <*> v <*> w) == (u <*> (v <*> w))

prop_applicativeHomomorphism :: (O -> O) -> O -> Bool
prop_applicativeHomomorphism f x = (f <$> pure x :: I) == pure (f x)

prop_applicativeInterchange :: IMap (O -> O) -> O -> Bool
prop_applicativeInterchange u y = (u <*> pure y) == (($ y) <$> u)

prop_empty :: Bool
prop_empty = lower (IMap.empty :: I) == IntMap.empty

prop_singleton :: Int -> Run O -> Bool
prop_singleton k r = lower (IMap.singleton k r) == lowerRun k r

prop_insert :: Int -> Run O -> I -> Bool
prop_insert k r m = lower (IMap.insert k r m) == IntMap.union (lowerRun k r) (lower m)

prop_delete :: Int -> Run () -> I -> Bool
prop_delete k r m = lower (IMap.delete k r m) == lower m IntMap.\\ lowerRun k r

prop_splitLE :: Int -> I -> Bool
prop_splitLE k m = (lower le, lower gt) == (le', gt') where
    (le, gt) = IMap.splitLE k m
    (lt, eq, gt') = IntMap.splitLookup k (lower m)
    le' = maybe id (IntMap.insert k) eq lt

prop_intersectionWith :: (O -> O -> O) -> I -> I -> Bool
prop_intersectionWith f l r = lower (IMap.intersectionWith f l r) == IntMap.intersectionWith f (lower l) (lower r)

prop_addToKeys :: Int -> I -> Bool
prop_addToKeys n m = lower (IMap.addToKeys n m) == IntMap.mapKeysMonotonic (n+) (lower m)

prop_lookup :: Int -> I -> Bool
prop_lookup k m = IMap.lookup k m == IntMap.lookup k (lower m)

prop_restrict :: Int -> Run () -> I -> Bool
prop_restrict k r m = lower (IMap.restrict k r m) == IntMap.intersection (lower m) (lowerRun k r)

prop_mapMaybe :: (O -> Maybe O) -> I -> Bool
prop_mapMaybe f m = lower (IMap.mapMaybe f m) == IntMap.mapMaybe f (lower m)

prop_null :: I -> Bool
prop_null m = IMap.null m == IntMap.null (lower m)

return []

main :: IO ()
main =
  (and <$> sequenceA [$quickCheckAll, List.main, Render.main])
  >>= bool exitFailure exitSuccess
