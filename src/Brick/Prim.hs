module Brick.Prim
  ( Render
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
  , withLens
  , readState
  , afterRendering
  )
where

import Brick.Prim.Internal

(<+>) :: Render a -> Render a -> Render a
(<+>) a b = hBox [(a, High), (b, High)]

(<<+) :: Render a -> Render a -> Render a
(<<+) a b = hBox [(a, High), (b, Low)]

(+>>) :: Render a -> Render a -> Render a
(+>>) a b = hBox [(a, Low), (b, High)]

(<=>) :: Render a -> Render a -> Render a
(<=>) a b = vBox [(a, High), (b, High)]

(<<=) :: Render a -> Render a -> Render a
(<<=) a b = vBox [(a, High), (b, Low)]

(=>>) :: Render a -> Render a -> Render a
(=>>) a b = vBox [(a, Low), (b, High)]
