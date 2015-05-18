module Brick.Scroll
  ( VScroll(..)
  , vScroll
  , vScrollToView

  , HScroll(..)
  , hScroll
  , hScrollToView
  )
where

import Brick.Core (SetSize(..))
import Brick.Prim (Prim(..))

data VScroll =
    VScroll { vScrollTop :: !Int
            , vScrollHeight :: !Int
            }

instance SetSize VScroll where
    setSize (_, h) vs = vs { vScrollHeight = h }

vScroll :: VScroll -> Prim -> Prim
vScroll vs p = Translate 0 (-1 * vScrollTop vs) $ VRelease p

vScrollToView :: Int -> VScroll -> VScroll
vScrollToView row vs =
    vs { vScrollTop = newTop }
    where
        newTop = if row < vScrollTop vs
                 then row
                 else if row >= vScrollTop vs + vScrollHeight vs
                      then row - vScrollHeight vs + 1
                      else vScrollTop vs

data HScroll =
    HScroll { hScrollLeft :: !Int
            , hScrollWidth :: !Int
            }

instance SetSize HScroll where
    setSize (w, _) hs = hs { hScrollWidth = w }

hScroll :: HScroll -> Prim -> Prim
hScroll hs p = Translate (-1 * hScrollLeft hs) 0 $ HRelease p

hScrollToView :: Int -> HScroll -> HScroll
hScrollToView col hs =
    hs { hScrollLeft = newLeft }
    where
        newLeft = if col < hScrollLeft hs
                  then col
                  else if col >= hScrollLeft hs + hScrollWidth hs
                       then col - hScrollWidth hs + 1
                       else hScrollLeft hs
