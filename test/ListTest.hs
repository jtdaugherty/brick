{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ListTest ( tests ) where

import Brick.Widgets.List

import Control.Applicative
import Control.Lens
import qualified Data.Vector as V
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck


tests :: TestTree
tests = testGroup "List" [ listReplaceTests ]

listReplaceTests :: TestTree
listReplaceTests = testGroup "listReplace" [listReplaceUnitTests, listReplaceProperties]

listReplaceUnitTests :: TestTree
listReplaceUnitTests = testGroup "Unit tests"
    [ testCase "with same list" $
          listReplace (V.fromList [1,2,3]) selectedList `assertList` ([1,2,3], Just 1)

    , testCase "with empty list" $
          listReplace V.empty selectedList `assertList` ([], Nothing)

    , testCase "empty list with nonempty list" $
          listReplace (V.fromList [1,2,3]) emptyList `assertList` ([1,2,3], Just 0)

    , testCase "insert one element in front of selection" $
          listReplace (V.fromList [1,0,2,3]) selectedList `assertList` ([1,0,2,3], Just 2)

    , testCase "insert one element after selection" $
          listReplace (V.fromList [1,2,0,3]) selectedList `assertList` ([1,2,0,3], Just 1)

    , testCase "delete one element in front of selection" $
          listReplace (V.fromList [2,3]) selectedList `assertList` ([2,3], Just 0)

    , testCase "delete one element after selection" $
          listReplace (V.fromList [1,2]) selectedList `assertList` ([1,2], Just 1)

    , testCase "deletion with multiple equal elements" $
          listReplace (V.fromList [2,2,3]) multiList `assertList` ([2,2,3], Just 1)

    , testCase "insertion with multiple equal elements" $
          listReplace (V.fromList [0,2,1,2,3]) selectedList `assertList` ([0,2,1,2,3], Just 3)
    ]

    where
        sampleList = list "list" (V.fromList [1, 2, 3]) 1 :: List Int
        selectedList = sampleList & listSelectedL .~ Just 1
        emptyList = list "empty" V.empty 1 :: List Int
        multiList = listInsert 0 2 selectedList -- [2,1,2,3]

listReplaceProperties :: TestTree
listReplaceProperties = testGroup "Properties"
    [ testProperty "selection always stays within bounds" $
          forAll listGen $ \lst ->
          forAll vecGen  $ \elems ->
              propReplaceBounds elems lst

    , testProperty "selected element stays the same" $
          forAll listGen $ \lst ->
          forAll vecGen  $ \elems ->
              propReplaceSelectionEq elems lst
    ]

propReplaceBounds :: (Eq e) => V.Vector e -> List e -> Property
propReplaceBounds elems lst = case listReplace elems lst ^. listSelectedL of
    Nothing -> property True
    Just idx -> counterexample ("selection: " ++ show idx) $ idx >= 0 .&&. idx < V.length elems

propReplaceSelectionEq :: (Show e, Eq e) => V.Vector e -> List e -> Property
propReplaceSelectionEq elems lst = case listSelectedElement lst of
    Nothing -> property Discard
    Just (_, e) -> e `V.elem` elems ==>
                   nothingOrEqual (listSelectedElement (listReplace elems lst))

        where
            nothingOrEqual Nothing = property True
            nothingOrEqual (Just (i, e')) = i >= 0 && i < V.length elems ==> e === e'

listGen :: Gen (List Int)
listGen = do
    elems <- vecGen
    selection <- if V.null elems
                    then return Nothing
                    else Just <$> choose (0, V.length elems - 1)
    return (list "list" elems 1 & listSelectedL .~ selection)

-- Required by QuickCheck
instance Show e => Show (List e) where
    show lst = show (lst^.listElementsL.to V.toList) ++ " !! " ++ show (lst^.listSelectedL)

vecGen :: Gen (V.Vector Int)
vecGen = V.fromList <$> arbitrary

assertList :: (Eq e, Show e) => List e -> ([e], Maybe Int) -> Assertion
assertList lst (elems, selection) = do
    assertEqual "contents" elems (lst^.listElementsL.to V.toList)
    assertEqual "selected element" selection (lst^.listSelectedL)
