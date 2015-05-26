{-# LANGUAGE OverloadedStrings #-}
module Brick.Border
  ( border
  , borderWithLabel

  , hBorder
  , hBorderWithLabel
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
import Brick.Center (hCenterWith)

border :: BorderStyle -> Prim a -> Prim a
border bs = border_ bs Nothing

borderWithLabel :: BorderStyle -> String -> Prim a -> Prim a
borderWithLabel bs label = border_ bs (Just label)

border_ :: BorderStyle -> Maybe String -> Prim a -> Prim a
border_ bs label wrapped = total
    where
        top = txt [bsCornerTL bs] <<+ hBorder_ bs label +>> txt [bsCornerTR bs]
        bottom = txt [bsCornerBL bs] <<+ hBorder bs +>> txt [bsCornerBR bs]
        middle = vBorder bs +>> wrapped <<+ vBorder bs
        total = top =>> middle <<= bottom

hBorder :: BorderStyle -> Prim a
hBorder bs = hBorder_ bs Nothing

hBorderWithLabel :: BorderStyle -> String -> Prim a
hBorderWithLabel bs label = hBorder_ bs (Just label)

hBorder_ :: BorderStyle -> Maybe String -> Prim a
hBorder_ bs label = hCenterWith (bsHorizontal bs) msg
    where
        msg = maybe (txt "") txt label

vBorder :: BorderStyle -> Prim a
vBorder = vFill . bsVertical

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
