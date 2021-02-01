{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick
import Brick.Widgets.Table

ui :: Widget ()
ui = table [AlignCenter, AlignRight]
           [ [txt "foo",   txt "bar"]
           , [txt "stuff", txt "more things"]
           , [txt "a",     txt "blah"]
           ]

main :: IO ()
main = simpleMain ui
