module Brick.Render
  ( Render
  , Priority(..)
  , (=>>), (<<=), (<=>)
  , (+>>), (<<+), (<+>)
  , ViewportType(..)

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
  , hRelease
  , vRelease
  , viewport
  , visible
  )
where

import Brick.Render.Internal

(<+>) :: Render -> Render -> Render
(<+>) a b = hBox [(a, High), (b, High)]

(<<+) :: Render -> Render -> Render
(<<+) a b = hBox [(a, High), (b, Low)]

(+>>) :: Render -> Render -> Render
(+>>) a b = hBox [(a, Low), (b, High)]

(<=>) :: Render -> Render -> Render
(<=>) a b = vBox [(a, High), (b, High)]

(<<=) :: Render -> Render -> Render
(<<=) a b = vBox [(a, High), (b, Low)]

(=>>) :: Render -> Render -> Render
(=>>) a b = vBox [(a, Low), (b, High)]
