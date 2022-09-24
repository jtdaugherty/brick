{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module List
  ( main
  )
where

import Prelude hiding (reverse, splitAt)

import Data.Foldable (find)
import Data.Function (on)
import qualified Data.List
import Data.Maybe (isNothing)
import Data.Monoid (Endo(..))
import Data.Proxy
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup((<>)))
#endif

import qualified Data.Sequence as Seq
import qualified Data.Vector as V
import Lens.Micro
import Test.QuickCheck

import Brick.Util (clamp)
import Brick.Widgets.List

instance (Arbitrary n, Arbitrary a) => Arbitrary (List n a) where
    arbitrary = list <$> arbitrary <*> (V.fromList <$> arbitrary) <*> pure 1

-- List move operations that never modify the underlying list
data ListMoveOp a =
    MoveUp
    | MoveDown
    | MoveBy Int
    | MoveTo Int
    | MoveToElement a
    | FindElement a
    deriving (Show)

instance Arbitrary a => Arbitrary (ListMoveOp a) where
    arbitrary =
        oneof [ pure MoveUp
              , pure MoveDown
              , MoveBy <$> arbitrary
              , MoveTo <$> arbitrary
              , MoveToElement <$> arbitrary
              , FindElement <$> arbitrary
              ]

-- List operations.  We don't have "page"-based movement operations
-- because these depend on render context (i.e. effect in EventM)
data ListOp a =
    Insert Int a
    | Remove Int
    | Replace Int [a]
    | Clear
    | Reverse
    | ListMoveOp (ListMoveOp a)
    deriving (Show)

instance Arbitrary a => Arbitrary (ListOp a) where
    arbitrary =
        frequency [ (1, Insert <$> arbitrary <*> arbitrary)
                  , (1, Remove <$> arbitrary)
                  , (1, Replace <$> arbitrary <*> arbitrary)
                  , (1, pure Clear)
                  , (1, pure Reverse)
                  , (6, arbitrary)
                  ]

-- Turn a ListOp into a List endomorphism
op :: Eq a => ListOp a -> List n a -> List n a
op (Insert i a) = listInsert i a
op (Remove i) = listRemove i
op (Replace i xs) =
    -- avoid setting index to Nothing
    listReplace (V.fromList xs) (Just i)
op Clear = listClear
op Reverse = listReverse
op (ListMoveOp mo) = moveOp mo

-- Turn a ListMoveOp into a List endomorphism
moveOp :: (Eq a) => ListMoveOp a -> List n a -> List n a
moveOp MoveUp = listMoveUp
moveOp MoveDown = listMoveDown
moveOp (MoveBy n) = listMoveBy n
moveOp (MoveTo n) = listMoveTo n
moveOp (MoveToElement a) = listMoveToElement a
moveOp (FindElement a) = listFindBy (== a)

applyListOps :: (Foldable t)
             => (op a -> List n a -> List n a)
             -> t (op a)
             -> List n a
             -> List n a
applyListOps f = appEndo . foldMap (Endo . f)

-- | Initial selection is always 0 (or Nothing for empty list)
prop_initialSelection :: [a] -> Bool
prop_initialSelection xs =
    list () (V.fromList xs) 1 ^. listSelectedL ==
        if null xs then Nothing else Just 0

-- list operations keep the selected index in bounds
prop_listOpsMaintainSelectedValid :: (Eq a)
                                  => [ListOp a]
                                  -> List n a
                                  -> Bool
prop_listOpsMaintainSelectedValid ops l =
    let l' = applyListOps op ops l
    in case l' ^. listSelectedL of
        -- either there is no selection and list is empty
        Nothing -> null l'
        -- or the selected index is valid
        Just i -> i >= 0 && i < length l'

-- reversing a list keeps the selected element the same
prop_reverseMaintainsSelectedElement :: (Eq a)
                                     => [ListOp a]
                                     -> List n a
                                     -> Bool
prop_reverseMaintainsSelectedElement ops l =
    -- apply some random list ops to (probably) set a selected element
    let l' = applyListOps op ops l
        l'' = listReverse l'
    in fmap snd (listSelectedElement l') == fmap snd (listSelectedElement l'')

-- reversing maintains size of list
prop_reverseMaintainsSizeOfList :: List n a -> Bool
prop_reverseMaintainsSizeOfList l =
    length l == length (listReverse l)

-- an inserted element may always be found at the given index
-- (when target index is clamped to 0 <= n <= len)
prop_insert :: (Eq a) => Int -> a -> List n a -> Bool
prop_insert i a l =
    let l' = listInsert i a l
        i' = clamp 0 (length l) i
    in listSelectedElement (listMoveTo i' l') == Just (i', a)

-- inserting anywhere always increases size of list by 1
prop_insertSize :: (Eq a) => Int -> a -> List n a -> Bool
prop_insertSize i a l =
    let l' = listInsert i a l
    in length l' == length l + 1

-- inserting an element and moving to it always succeeds and
-- the selected element is the one we inserted.
--
-- The index is not necessarily the index we inserted at, because
-- the element could be present in the original list.  So we don't
-- check that.
--
prop_insertMoveTo :: (Eq a) => [ListOp a] -> List n a -> Int -> a -> Bool
prop_insertMoveTo ops l i a =
    let l' = listInsert i a (applyListOps op ops l)
        sel = listSelectedElement (listMoveToElement a l')
    in fmap snd sel == Just a

-- inserting an element and repeatedly seeking it always
-- reaches the element we inserted, at the index where we
-- inserted it.
--
prop_insertFindBy :: (Eq a) => [ListOp a] -> List n a -> Int -> a -> Bool
prop_insertFindBy ops l i a =
    let l' = applyListOps op ops l
        l'' = set listSelectedL Nothing . listInsert i a $ l'
        seeks = converging ((==) `on` (^. listSelectedL)) (listFindBy (== a)) l''
        i' = clamp 0 (length l') i -- we can't have inserted past len
    in (find ((== Just i') . (^. listSelectedL)) seeks >>= listSelectedElement) == Just (i', a)

-- inserting then deleting always yields a list with the original elems
prop_insertRemove :: (Eq a) => Int -> a -> List n a -> Bool
prop_insertRemove i a l =
    let i' = clamp 0 (length l) i
        l' = listInsert i' a l -- pre-clamped
        l'' = listRemove i' l'
    in l'' ^. listElementsL == l ^. listElementsL

-- deleting in-bounds always reduces size of list by 1
-- deleting out-of-bounds never changes list size
prop_remove :: Int -> List n a -> Bool
prop_remove i l =
    let len = length l
        i' = clamp 0 (len - 1) i
        test
            | len > 0 && i == i' = (== len - 1)  -- i is in bounds
            | otherwise = (== len)               -- i is out of bounds
    in test (length (listRemove i l))

-- deleting an element and re-inserting it at same position
-- gives the original list elements
prop_removeInsert :: (Eq a) => Int -> List n a -> Bool
prop_removeInsert i l =
    let sel = listSelectedElement (listMoveTo i l)
        l' = maybe id (\(i', a) -> listInsert i' a . listRemove i') sel l
    in l' ^. listElementsL == l ^. listElementsL

-- Apply @f@ until @test a (f a) == True@, then return @a@.
converge :: (a -> a -> Bool) -> (a -> a) -> a -> a
converge test f = last . converging test f

-- Apply @f@ until @test a (f a) == True@, returning the start,
-- intermediate and final values as a list.
converging :: (a -> a -> Bool) -> (a -> a) -> a -> [a]
converging test f a
    | test a (f a) = [a]
    | otherwise = a : converging test f (f a)

-- listMoveUp always reaches 0 (or list is empty)
prop_moveUp :: (Eq a) => [ListOp a] -> List n a -> Bool
prop_moveUp ops l =
    let l' = applyListOps op ops l
        l'' = converge ((==) `on` (^. listSelectedL)) listMoveUp l'
        len = length l''
    in maybe (len == 0) (== 0) (l'' ^. listSelectedL)

-- listMoveDown always reaches end of list (or list is empty)
prop_moveDown :: (Eq a) => [ListOp a] -> List n a -> Bool
prop_moveDown ops l =
    let l' = applyListOps op ops l
        l'' = converge ((==) `on` (^. listSelectedL)) listMoveDown l'
        len = length l''
    in maybe (len == 0) (== len - 1) (l'' ^. listSelectedL)

-- move ops never change the list
prop_moveOpsNeverChangeList :: (Eq a) => [ListMoveOp a] -> List n a -> Bool
prop_moveOpsNeverChangeList ops l =
    let l' = applyListOps moveOp ops l
    in l' ^. listElementsL == l ^. listElementsL

-- If the list is empty, empty selection is used.
-- Otherwise, if the specified selected index is not in list bounds,
-- zero is used instead.
prop_replaceSetIndex :: (Eq a)
                     => [ListOp a]
                     -> List n a
                     -> [a]
                     -> Int
                     -> Bool
prop_replaceSetIndex ops l xs i =
    let v = V.fromList xs
        l' = applyListOps op ops l
        l'' = listReplace v (Just i) l'
        i' = clamp 0 (length v - 1) i
        inBounds = i == i'
    in l'' ^. listSelectedL == case (null v, inBounds) of
        (True, _) -> Nothing
        (False, True) -> Just i
        (False, False) -> Just 0

-- Replacing with no index always clears the index
prop_replaceNoIndex :: (Eq a) => [ListOp a] -> List n a -> [a] -> Bool
prop_replaceNoIndex ops l xs =
    let v = V.fromList xs
        l' = applyListOps op ops l
    in isNothing (listReplace v Nothing l' ^. listSelectedL)

-- | Move the list selected index. If the index is `Just x`, adjust by the
-- specified amount; if it is `Nothing` (i.e. there is no selection) and the
-- direction is positive, set to `Just 0` (first element), otherwise set to
-- `Just (length - 1)` (last element). Subject to validation.
prop_moveByWhenNoSelection :: List n a -> Int -> Property
prop_moveByWhenNoSelection l amt =
    let l' = l & listSelectedL .~ Nothing
        len = length l
        expected = if amt > 0 then 0 else len - 1
    in len > 0 ==> listMoveBy amt l' ^. listSelectedL == Just expected

splitAtLength :: (Foldable t, Splittable t) => t a -> Int -> Bool
splitAtLength l i =
    let len = length l
        (h, t) = splitAt i l
    in length h + length t == len && length h == clamp 0 len i

splitAtAppend :: (Splittable t, Semigroup (t a), Eq (t a))
              => t a -> Int -> Bool
splitAtAppend l i = uncurry (<>) (splitAt i l) == l

prop_splitAtLength_Vector :: [a] -> Int -> Bool
prop_splitAtLength_Vector = splitAtLength . V.fromList

prop_splitAtAppend_Vector :: (Eq a) => [a] -> Int -> Bool
prop_splitAtAppend_Vector = splitAtAppend . V.fromList

prop_splitAtLength_Seq :: [a] -> Int -> Bool
prop_splitAtLength_Seq = splitAtLength . Seq.fromList

prop_splitAtAppend_Seq :: (Eq a) => [a] -> Int -> Bool
prop_splitAtAppend_Seq = splitAtAppend . Seq.fromList


reverseSingleton :: forall t a. (Reversible t, Applicative t, Eq (t a))
                 => Proxy t -> a -> Bool
reverseSingleton _ a =
    let l = pure a :: t a
    in reverse l == l

reverseAppend :: (Reversible t, Semigroup (t a), Eq (t a))
              => t a -> t a -> Bool
reverseAppend l1 l2 =
  reverse (l1 <> l2) == reverse l2 <> reverse l1

prop_reverseSingleton_Vector :: (Eq a) => a -> Bool
prop_reverseSingleton_Vector = reverseSingleton (Proxy :: Proxy V.Vector)

prop_reverseAppend_Vector :: (Eq a) => [a] -> [a] -> Bool
prop_reverseAppend_Vector l1 l2 =
    reverseAppend (V.fromList l1) (V.fromList l2)

prop_reverseSingleton_Seq :: (Eq a) => a -> Bool
prop_reverseSingleton_Seq = reverseSingleton (Proxy :: Proxy Seq.Seq)

prop_reverseAppend_Seq :: (Eq a) => [a] -> [a] -> Bool
prop_reverseAppend_Seq l1 l2 =
    reverseAppend (Seq.fromList l1) (Seq.fromList l2)

-- Laziness tests.  Here we create a custom container type
-- that we use to ensure certain operations do not cause the
-- whole container to be evaluated.
--
newtype L a = L [a]
            deriving (Functor, Foldable, Traversable, Semigroup)

instance Splittable L where
    splitAt i (L xs) = over both L (Data.List.splitAt i xs)

-- moveBy positive amount does not evaluate 'length'
prop_moveByPosLazy :: Bool
prop_moveByPosLazy =
    let v = L (1:2:3:4:undefined) :: L Int
        l = list () v 1
        l' = listMoveBy 1 l
    in l' ^. listSelectedL == Just 1

-- listFindBy is lazy
prop_findByLazy :: Bool
prop_findByLazy =
    let v = L (1:2:3:4:undefined) :: L Int
        l = list () v 1 & listSelectedL .~ Nothing
        l' = listFindBy even l
        l'' = listFindBy even l'
    in l' ^. listSelectedL == Just 1 &&
       l'' ^. listSelectedL == Just 3

prop_listSelectedElement_lazy :: Bool
prop_listSelectedElement_lazy =
    let v = L (1:2:3:4:undefined) :: L Int
        l = list () v 1 & listSelectedL .~ Just 3
    in listSelectedElement l == Just (3, 4)

prop_listSelectedElementL_lazy :: Bool
prop_listSelectedElementL_lazy =
    let v = L (1:2:3:4:undefined) :: L Int
        l = list () v 1 & listSelectedL .~ Just 3
    in over listSelectedElementL (*2) l ^? listSelectedElementL == Just 8

return []

main :: IO Bool
main = $quickCheckAll
