module Main where

import Data.Monoid ((<>))
import Brick

ui :: Widget ()
ui = strWrap $ "Hello, world! This line is long enough that " <>
               "it's likely to wrap on your terminal if your window " <>
               "isn't especially wide. Try narrowing and widening " <>
               "the window to see what happens to this text."

main :: IO ()
main = simpleMain ui
