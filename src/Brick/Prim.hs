{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
module Brick.Prim
  ( Prim(..)
  , Priority(..)
  , (=>>), (<<=), (<=>)
  , (+>>), (<<+), (<+>)
  )
where

import Control.Lens (Lens')
import Data.String (IsString(..))
import Graphics.Vty (DisplayRegion, Image, Attr)

import Brick.Core (Location(..), Name(..))

data Priority = High | Low
              deriving (Show, Eq)

data Prim a = Txt !String
            | HPad !Char
            | VPad !Char
            | HFill !Char
            | VFill !Char
            | HBox ![(Prim a, Priority)]
            | VBox ![(Prim a, Priority)]
            | HLimit !Int !(Prim a)
            | VLimit !Int !(Prim a)
            | UseAttr !Attr !(Prim a)
            | Raw !Image
            | Translate !Int !Int !(Prim a)
            | CropLeftBy !Int !(Prim a)
            | CropRightBy !Int !(Prim a)
            | CropTopBy !Int !(Prim a)
            | CropBottomBy !Int !(Prim a)
            | ShowCursor !Name !Location !(Prim a)
            | SetSize (DisplayRegion -> a -> a) !(Prim a)
            | HRelease !(Prim a)
            | VRelease !(Prim a)
            | forall b. WithState (Lens' a b) (b -> Prim b)

instance IsString (Prim a) where
    fromString = Txt

(<+>) :: Prim a -> Prim a -> Prim a
(<+>) a b = HBox [(a, High), (b, High)]

(<<+) :: Prim a -> Prim a -> Prim a
(<<+) a b = HBox [(a, High), (b, Low)]

(+>>) :: Prim a -> Prim a -> Prim a
(+>>) a b = HBox [(a, Low), (b, High)]

(<=>) :: Prim a -> Prim a -> Prim a
(<=>) a b = VBox [(a, High), (b, High)]

(<<=) :: Prim a -> Prim a -> Prim a
(<<=) a b = VBox [(a, High), (b, Low)]

(=>>) :: Prim a -> Prim a -> Prim a
(=>>) a b = VBox [(a, Low), (b, High)]
