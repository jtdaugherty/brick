{-# LANGUAGE BangPatterns #-}
module Brick.Prim
  ( Prim(..)
  , Priority(..)
  , (=>>), (<<=), (<=>)
  , (+>>), (<<+), (<+>)
  )
where

import Data.String (IsString(..))
import Graphics.Vty (Image, Attr)

import Brick.Core (Location(..), Name(..))

data Priority = High | Low
              deriving (Show, Eq)

data Prim = Txt !String
          | HPad !Char
          | VPad !Char
          | HFill !Char
          | VFill !Char
          | HBox ![(Prim, Priority)]
          | VBox ![(Prim, Priority)]
          | HLimit !Int !Prim
          | VLimit !Int !Prim
          | UseAttr !Attr !Prim
          | Raw !Image
          | Translate !Int !Int !Prim
          | CropLeftBy !Int !Prim
          | CropRightBy !Int !Prim
          | CropTopBy !Int !Prim
          | CropBottomBy !Int !Prim
          | ShowCursor !Name !Location !Prim
          | GetSize !Name !Prim
          | HRelease !Prim
          | VRelease !Prim
          deriving Show

instance IsString Prim where
    fromString = Txt

(<+>) :: Prim -> Prim -> Prim
(<+>) a b = HBox [(a, High), (b, High)]

(<<+) :: Prim -> Prim -> Prim
(<<+) a b = HBox [(a, High), (b, Low)]

(+>>) :: Prim -> Prim -> Prim
(+>>) a b = HBox [(a, Low), (b, High)]

(<=>) :: Prim -> Prim -> Prim
(<=>) a b = VBox [(a, High), (b, High)]

(<<=) :: Prim -> Prim -> Prim
(<<=) a b = VBox [(a, High), (b, Low)]

(=>>) :: Prim -> Prim -> Prim
(=>>) a b = VBox [(a, Low), (b, High)]
