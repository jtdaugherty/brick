{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick
import Brick.Widgets.Table

ui :: Widget ()
ui = table [[txt "foo", txt "bar"], [txt "stuff", txt "more things"]]

main :: IO ()
main = simpleMain ui
