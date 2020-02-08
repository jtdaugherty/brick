{-# LANGUAGE CPP #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick
import Text.Wrap (defaultWrapSettings, preserveIndentation)

ui :: Widget ()
ui =
    t1 <=> (padTop (Pad 1) t2)
    where
      t1 = strWrap $ "Hello, world! This line is long enough that " <>
                     "it's likely to wrap on your terminal if your window " <>
                     "isn't especially wide. Try narrowing and widening " <>
                     "the window to see what happens to this text."
      settings = defaultWrapSettings { preserveIndentation = True }
      t2 = strWrapWith settings $
          "This text wraps\n" <>
          "   with different settings to preserve indentation\n" <>
          "   so that long lines wrap in nicer way."

main :: IO ()
main = simpleMain ui
