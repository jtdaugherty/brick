{-# LANGUAGE OverloadedStrings #-}
-- | This module provides styles for borders as used in terminal
-- applications. Your mileage may vary on some of the fancier styles
-- due to varying support for some border characters in the fonts your
-- users may be using. Because of this, we provide the 'ascii' style in
-- addition to the Unicode styles. The 'unicode' style is also a safe
-- bet.
--
-- To use these in your widgets, see
-- 'Brick.Widgets.Core.withBorderStyle'. By default, widgets rendered
-- without a specified border style use 'ascii' via the 'Default'
-- instance provided by 'BorderStyle'.
module Brick.Widgets.Border.Style
  ( BorderStyle(..)
  , borderStyleFromChar
  , ascii
  , unicode
  , unicodeBold
  , unicodeRounded
  )
where

import Data.Default

-- | A border style for use in any widget that needs to render borders
-- in a consistent style.
data BorderStyle =
    BorderStyle { bsCornerTL :: Char
                -- ^ Top-left corner character
                , bsCornerTR :: Char
                -- ^ Top-right corner character
                , bsCornerBR :: Char
                -- ^ Bottom-right corner character
                , bsCornerBL :: Char
                -- ^ Bottom-left corner character
                , bsIntersectionFull :: Char
                -- ^ Full intersection (cross)
                , bsIntersectionL :: Char
                -- ^ Left side of a horizontal border intersecting a vertical one
                , bsIntersectionR :: Char
                -- ^ Right side of a horizontal border intersecting a vertical one
                , bsIntersectionT :: Char
                -- ^ Top of a vertical border intersecting a horizontal one
                , bsIntersectionB :: Char
                -- ^ Bottom of a vertical border intersecting a horizontal one
                , bsHorizontal :: Char
                -- ^ Horizontal border character
                , bsVertical :: Char
                -- ^ Vertical border character
                }
                deriving (Show, Read)

instance Default BorderStyle where
    def = ascii

-- | Make a border style using the specified character everywhere.
borderStyleFromChar :: Char -> BorderStyle
borderStyleFromChar c =
    BorderStyle c c c c c c c c c c c

-- |An ASCII border style which will work in any terminal.
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

-- |A unicode border style with real corner and intersection characters.
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

-- |A unicode border style in a bold typeface.
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

-- |A unicode border style with rounded corners.
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
