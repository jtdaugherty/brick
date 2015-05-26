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
  )
where

import Brick.Prim.Internal

(<+>) :: Prim a -> Prim a -> Prim a
(<+>) a b = hBox [(a, High), (b, High)]

(<<+) :: Prim a -> Prim a -> Prim a
(<<+) a b = hBox [(a, High), (b, Low)]

(+>>) :: Prim a -> Prim a -> Prim a
(+>>) a b = hBox [(a, Low), (b, High)]

(<=>) :: Prim a -> Prim a -> Prim a
(<=>) a b = vBox [(a, High), (b, High)]

(<<=) :: Prim a -> Prim a -> Prim a
(<<=) a b = vBox [(a, High), (b, Low)]

(=>>) :: Prim a -> Prim a -> Prim a
(=>>) a b = vBox [(a, Low), (b, High)]
