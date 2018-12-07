{-# LANGUAGE TemplateHaskell #-}

module List
  (
    main
  ) where

import Data.Function (on)
import Data.Monoid (Endo(..))

import qualified Data.Vector as V
import Lens.Micro
import Test.QuickCheck

import Brick.Util (clamp)
import Brick.Widgets.List

instance (Arbitrary n, Arbitrary a) => Arbitrary (List n a) where
  arbitrary = list <$> arbitrary <*> (V.fromList <$> arbitrary) <*> pure 1


-- List move operations that never modify the underlying list
data ListMoveOp a
  = MoveUp
  | MoveDown
  | MoveBy Int
  | MoveTo Int
  | MoveToElement a
  deriving (Show)

instance Arbitrary a => Arbitrary (ListMoveOp a) where
  arbitrary = oneof
    [ pure MoveUp
    , pure MoveDown
    , MoveBy <$> arbitrary
    , MoveTo <$> arbitrary
    , MoveToElement <$> arbitrary
    ]

-- List operations.  We don't have "page"-based movement operations
-- because these depend on render context (i.e. effect in EventM)
data ListOp a
  = Insert Int a
  | Remove Int
  | Replace Int [a]
  | Clear
  | Reverse
  | ListMoveOp (ListMoveOp a)
  deriving (Show)

instance Arbitrary a => Arbitrary (ListOp a) where
  arbitrary = frequency
    [ (1, Insert <$> arbitrary <*> arbitrary)
    , (1, Remove <$> arbitrary)
    , (1, Replace <$> arbitrary <*> arbitrary)
    , (1, pure Clear)
    , (1, pure Reverse)
    , (5, arbitrary)
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

applyListOps
  :: (Foldable t)
  => (op a -> List n a -> List n a) -> t (op a) -> List n a -> List n a
applyListOps f = appEndo . foldMap (Endo . f)

-- list operations keep the selected index in bounds
prop_listOpsMaintainSelectedValid
  :: (Eq a) => [ListOp a] -> List n a -> Bool
prop_listOpsMaintainSelectedValid ops l =
  let l' = applyListOps op ops l
  in
    case l' ^. listSelectedL of
      -- either there is no selection and list is empty
      Nothing -> null (l' ^. listElementsL)
      -- or the selected index is valid
      Just i -> i >= 0 && i < length (l' ^. listElementsL)

-- reversing a list keeps the selected element the same
prop_reverseMaintainsSelectedElement
  :: (Eq a) => [ListOp a] -> List n a -> Bool
prop_reverseMaintainsSelectedElement ops l =
  let
    -- apply some random list ops to (probably) set a selected element
    l' = applyListOps op ops l
    l'' = listReverse l'
  in
    fmap snd (listSelectedElement l') == fmap snd (listSelectedElement l'')

-- an inserted element may always be found at the given index
-- (when target index is clamped to 0 <= n <= len)
prop_insert :: (Eq a) => Int -> a -> List n a -> Bool
prop_insert i a l =
  let
    l' = listInsert i a l
    i' = clamp 0 (length (l ^. listElementsL)) i
  in
    listSelectedElement (listMoveTo i' l') == Just (i', a)

-- inserting anywhere always increases size of list by 1
prop_insertSize :: (Eq a) => Int -> a -> List n a -> Bool
prop_insertSize i a l =
  let
    l' = listInsert i a l
  in
    length (l' ^. listElementsL) == length (l ^. listElementsL) + 1

-- inserting an element and moving to it always succeeds and
-- the selected element is the one we inserted.
--
-- The index is not necessarily the index we inserted at, because
-- the element could be present in the original list.  So we don't
-- check that.
--
prop_insertMoveTo :: (Eq a) => [ListOp a] -> List n a -> Int -> a -> Bool
prop_insertMoveTo ops l i a =
  let
    l' = listInsert i a (applyListOps op ops l)
    sel = listSelectedElement (listMoveToElement a l')
  in
    fmap snd sel == Just a

-- inserting then deleting always yields a list with the original elems
prop_insertRemove :: (Eq a) => Int -> a -> List n a -> Bool
prop_insertRemove i a l =
  let
    i' = clamp 0 (length (l ^. listElementsL)) i
    l' = listInsert i' a l -- pre-clamped
    l'' = listRemove i' l'
  in
    l'' ^. listElementsL == l ^. listElementsL

-- deleting in-bounds always reduces size of list by 1
-- deleting out-of-bounds never changes list size
prop_remove :: Int -> List n a -> Bool
prop_remove i l =
  let
    len = length (l ^. listElementsL)
    i' = clamp 0 (len - 1) i
    test
      | len > 0 && i == i' = (== len - 1)  -- i is in bounds
      | otherwise = (== len)               -- i is out of bounds
  in
    test (length (listRemove i l ^. listElementsL))

-- deleting an element and re-inserting it at same position
-- gives the original list elements
prop_removeInsert :: (Eq a) => Int -> List n a -> Bool
prop_removeInsert i l =
  let
    sel = listSelectedElement (listMoveTo i l)
    l' = maybe id (\(i', a) -> listInsert i' a . listRemove i') sel l
  in
    l' ^. listElementsL == l ^. listElementsL

converge :: (a -> a -> Bool) -> (a -> a) -> a -> a
converge test f a
  | test (f a) a = a
  | otherwise = converge test f (f a)

-- listMoveUp always reaches 0 (or list is empty)
prop_moveUp :: (Eq a) => [ListOp a] -> List n a -> Bool
prop_moveUp ops l =
  let
    l' = applyListOps op ops l
    l'' = converge ((==) `on` (^. listSelectedL)) listMoveUp l'
    len = length (l'' ^. listElementsL)
  in
    maybe (len == 0) (== 0) (l'' ^. listSelectedL)

-- listMoveDown always reaches end of list (or list is empty)
prop_moveDown :: (Eq a) => [ListOp a] -> List n a -> Bool
prop_moveDown ops l =
  let
    l' = applyListOps op ops l
    l'' = converge ((==) `on` (^. listSelectedL)) listMoveDown l'
    len = length (l'' ^. listElementsL)
  in
    maybe (len == 0) (== len - 1) (l'' ^. listSelectedL)

-- move ops never change the list
prop_moveOpsNeverChangeList :: (Eq a) => [ListMoveOp a] -> List n a -> Bool
prop_moveOpsNeverChangeList ops l =
  let
    l' = applyListOps moveOp ops l
  in
    l' ^. listElementsL == l ^. listElementsL


return []

main :: IO Bool
main = $quickCheckAll
