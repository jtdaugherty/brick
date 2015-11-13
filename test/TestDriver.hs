module Main where

import qualified ListTest as List

import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [List.tests]
