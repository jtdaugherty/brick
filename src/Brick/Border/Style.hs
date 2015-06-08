{-# LANGUAGE OverloadedStrings #-}
module Brick.Border.Style
  ( BorderStyle(..)
  , ascii
  , unicode
  , unicodeBold
  , unicodeRounded
  )
where

import Data.Default

-- Corners start from top left and go clockwise.  Intersections are:
-- full, left, right, top, bottom.
data BorderStyle =
    BorderStyle { bsCornerTL :: Char
                , bsCornerTR :: Char
                , bsCornerBR :: Char
                , bsCornerBL :: Char
                , bsIntersectionFull :: Char
                , bsIntersectionL :: Char
                , bsIntersectionR :: Char
                , bsIntersectionT :: Char
                , bsIntersectionB :: Char
                , bsHorizontal :: Char
                , bsVertical :: Char
                }

instance Default BorderStyle where
    def = ascii

-- |An ASCII bs which will work in any terminal.
ascii :: BorderStyle
ascii =
    BorderStyle { bsCornerTL = '+'
                , bsCornerTR = '+'
                , bsCornerBR = '+'
                , bsCornerBL = '+'
                , bsIntersectionFull = '+'
                , bsIntersectionL = '+'
                , bsIntersectionR = '+'
                , bsIntersectionT = '+'
                , bsIntersectionB = '+'
                , bsHorizontal = '-'
                , bsVertical = '|'
                }

unicode :: BorderStyle
unicode =
    BorderStyle { bsCornerTL = '┌'
                , bsCornerTR = '┐'
                , bsCornerBR = '┘'
                , bsCornerBL = '└'
                , bsIntersectionFull = '┼'
                , bsIntersectionL = '├'
                , bsIntersectionR = '┤'
                , bsIntersectionT = '┬'
                , bsIntersectionB = '┴'
                , bsHorizontal = '─'
                , bsVertical = '│'
                }

unicodeBold :: BorderStyle
unicodeBold =
    BorderStyle { bsCornerTL = '┏'
                , bsCornerTR = '┓'
                , bsCornerBR = '┛'
                , bsCornerBL = '┗'
                , bsIntersectionFull = '╋'
                , bsIntersectionL = '┣'
                , bsIntersectionR = '┫'
                , bsIntersectionT = '┳'
                , bsIntersectionB = '┻'
                , bsHorizontal = '━'
                , bsVertical = '┃'
                }

unicodeRounded :: BorderStyle
unicodeRounded =
    BorderStyle { bsCornerTL = '╭'
                , bsCornerTR = '╮'
                , bsCornerBR = '╯'
                , bsCornerBL = '╰'
                , bsIntersectionFull = '┼'
                , bsIntersectionL = '├'
                , bsIntersectionR = '┤'
                , bsIntersectionT = '┬'
                , bsIntersectionB = '┴'
                , bsHorizontal = '─'
                , bsVertical = '│'
                }
