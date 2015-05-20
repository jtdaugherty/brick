module Brick.Scroll
  ( VScroll(..)
  , vScroll
  , vScrollToView

  , HScroll(..)
  , hScroll
  , hScrollToView
  )
where

import Brick.Core (SetSize(..), Location(..))
import Brick.Prim (Prim(..))

data VScroll =
    VScroll { vScrollTop :: !Int
            , vScrollHeight :: !Int
            }

instance SetSize VScroll where
    setSize (_, h) vs = vs { vScrollHeight = h }

vScroll :: VScroll -> Prim a -> Prim a
vScroll vs p = Translate (Location (0, -1 * vScrollTop vs)) $ VRelease p

vScrollToView :: (Int, Int) -> VScroll -> VScroll
vScrollToView (reqTop, reqHeight) vs =
    vs { vScrollTop = newTop }
    where
        -- cases:
        -- window is bigger than visible area -> scroll to top of requested region
        -- else
        --   top is before current top -> top
        --   top is below current bottom -> scroll so that requested bottom is new bottom
        --   bottom is below current bottom -> scroll so that requested bottom is new bottom
        --   else do nothing
        curBottom = curTop + vScrollHeight vs -- XXX should be - 1 more, too?
        curTop = vScrollTop vs
        reqBottom = reqTop + reqHeight
        newTop = if reqTop < curTop
                 then reqTop
                 else if reqTop > curBottom || reqBottom > curBottom
                      then reqBottom - vScrollHeight vs
                      else curTop

data HScroll =
    HScroll { hScrollLeft :: !Int
            , hScrollWidth :: !Int
            }

instance SetSize HScroll where
    setSize (w, _) hs = hs { hScrollWidth = w }

hScroll :: HScroll -> Prim a -> Prim a
hScroll hs p = Translate (Location (-1 * hScrollLeft hs, 0)) $ HRelease p

hScrollToView :: (Int, Int) -> HScroll -> HScroll
hScrollToView (reqLeft, reqWidth) hs =
    hs { hScrollLeft = newLeft }
    where
        curRight = curLeft + hScrollWidth hs
        curLeft = hScrollLeft hs
        reqRight = reqLeft + reqWidth
        newLeft = if reqLeft < curLeft
                  then reqLeft
                  else if reqLeft > curRight || reqRight > curRight
                       then reqRight - hScrollWidth hs
                       else curLeft
