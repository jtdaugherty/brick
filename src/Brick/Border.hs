{-# LANGUAGE OverloadedStrings #-}
module Brick.Border
  ( bordered
  , borderedWithLabel

  , hBorder
  , vBorder

  , BorderStyle(..)
  , ascii
  , unicode
  , unicodeBold
  , unicodeRounded
  )
where

import Data.Default

import Brick.Prim
import Brick.Center (hCenteredWith)

bordered :: BorderStyle -> Prim a -> Prim a
bordered = bordered_ Nothing

borderedWithLabel :: String -> BorderStyle -> Prim a -> Prim a
borderedWithLabel label = bordered_ (Just label)

bordered_ :: Maybe String -> BorderStyle -> Prim a -> Prim a
bordered_ label bs wrapped = total
    where
        labelStr = maybe (Txt "") Txt label
        top = Txt [bsCornerTL bs] <<+ hCenteredWith (bsHorizontal bs) labelStr +>> Txt [bsCornerTR bs]
        bottom = Txt [bsCornerBL bs] <<+ hBorder bs +>> Txt [bsCornerBR bs]
        middle = vBorder bs +>> wrapped <<+ vBorder bs
        total = top =>> middle <<= bottom

hBorder :: BorderStyle -> Prim a
hBorder = HFill . bsHorizontal

vBorder :: BorderStyle -> Prim a
vBorder = VFill . bsVertical

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
    def = unicode

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
