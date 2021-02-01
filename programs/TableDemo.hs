{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (center)

ui :: Widget ()
ui = center $ renderTable outerTable

innerTable :: Table ()
innerTable =
    surroundingBorder False $
    table [ [txt "inner", txt "table"]
          , [txt "is",    txt "here"]
          ]

outerTable :: Table ()
outerTable =
    alignCenter 1 $
    alignRight 2 $
    alignMiddle 2 $
    table [ [txt "Left",             txt "Center",      txt "Right"]
          , [txt "X",                txt "Some things", txt "A"]
          , [renderTable innerTable, txt "are",         txt "B"]
          , [txt "Z",                txt "centered",    txt "C"]
          ]

main :: IO ()
main = simpleMain ui
