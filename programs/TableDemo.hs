{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick
import Brick.Widgets.Table

ui :: Widget ()
ui = table [AlignLeft, AlignCenter, AlignRight]
           [ [txt "Left",  txt "Center",      txt "Right"]
           , [txt "X",     txt "Some things", txt "A"]
           , [txt "Y",     txt "are",         txt "B"]
           , [txt "Z",     txt "centered",    txt "C"]
           ]

main :: IO ()
main = simpleMain ui
