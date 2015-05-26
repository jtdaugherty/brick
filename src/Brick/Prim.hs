{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
module Brick.Prim
  ( Prim
  , Priority(..)
  , (=>>), (<<=), (<=>)
  , (+>>), (<<+), (<+>)
  , txt
  , hPad
  , vPad
  , hFill
  , vFill
  , hBox
  , vBox
  , hLimit
  , vLimit
  , useAttr
  , raw
  , translate
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy
  , showCursor
  , saveSize
  , hRelease
  , vRelease
  , with
  , readState

  -- xxx for now
  , Primitive(..)
  )
where

import Control.Lens (Lens')
import Control.Monad.Trans.State.Lazy
import Data.String (IsString(..))
import Graphics.Vty (DisplayRegion, Image, Attr)

import Brick.Core (Location(..), CursorName(..))

data Priority = High | Low
              deriving (Show, Eq)

type Prim a = State a (Primitive a)

data Primitive a = Txt !String
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
                 | Translate !Location !(Prim a)
                 | CropLeftBy !Int !(Prim a)
                 | CropRightBy !Int !(Prim a)
                 | CropTopBy !Int !(Prim a)
                 | CropBottomBy !Int !(Prim a)
                 | ShowCursor !CursorName !Location !(Prim a)
                 | SaveSize (DisplayRegion -> a -> a) !(Prim a)
                 | HRelease !(Prim a)
                 | VRelease !(Prim a)
                 | forall b. With (Lens' a b) (Prim b)
                 | ReadState (a -> Prim a)

instance IsString (Prim a) where
    fromString = return . Txt

(<+>) :: Prim a -> Prim a -> Prim a
(<+>) a b = return $ HBox [(a, High), (b, High)]

(<<+) :: Prim a -> Prim a -> Prim a
(<<+) a b = return $ HBox [(a, High), (b, Low)]

(+>>) :: Prim a -> Prim a -> Prim a
(+>>) a b = return $ HBox [(a, Low), (b, High)]

(<=>) :: Prim a -> Prim a -> Prim a
(<=>) a b = return $ VBox [(a, High), (b, High)]

(<<=) :: Prim a -> Prim a -> Prim a
(<<=) a b = return $ VBox [(a, High), (b, Low)]

(=>>) :: Prim a -> Prim a -> Prim a
(=>>) a b = return $ VBox [(a, Low), (b, High)]

txt :: String -> Prim a
txt = return . Txt

hPad :: Char -> Prim a
hPad = return . HPad

vPad :: Char -> Prim a
vPad = return . VPad

hFill :: Char -> Prim a
hFill = return . HFill

vFill :: Char -> Prim a
vFill = return . VFill

hBox :: [(Prim a, Priority)] -> Prim a
hBox = return . HBox

vBox :: [(Prim a, Priority)] -> Prim a
vBox = return . VBox

hLimit :: Int -> Prim a -> Prim a
hLimit l p = return $ HLimit l p

vLimit :: Int -> Prim a -> Prim a
vLimit l p = return $ VLimit l p

useAttr :: Attr -> Prim a -> Prim a
useAttr a p = return $ UseAttr a p

raw :: Image -> Prim a
raw = return . Raw

translate :: Location -> Prim a -> Prim a
translate l p = return $ Translate l p

cropLeftBy :: Int -> Prim a -> Prim a
cropLeftBy a p = return $ CropLeftBy a p

cropRightBy :: Int -> Prim a -> Prim a
cropRightBy a p = return $ CropRightBy a p

cropTopBy :: Int -> Prim a -> Prim a
cropTopBy a p = return $ CropTopBy a p

cropBottomBy :: Int -> Prim a -> Prim a
cropBottomBy a p = return $ CropBottomBy a p

showCursor :: CursorName -> Location -> Prim a -> Prim a
showCursor n l p = return $ ShowCursor n l p

saveSize :: (DisplayRegion -> a -> a) -> Prim a -> Prim a
saveSize f p = return $ SaveSize f p

hRelease :: Prim a -> Prim a
hRelease = return . HRelease

vRelease :: Prim a -> Prim a
vRelease = return . VRelease

with :: (Lens' a b) -> Prim b -> Prim a
with l p = return $ With l p

readState :: (a -> Prim a) -> Prim a
readState = return . ReadState
