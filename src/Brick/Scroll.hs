module Brick.Scroll
  ( Scrollable(..)

  , VScroll
  , vScroll

  , HScroll
  , hScroll

  , scrollToView
  )
where

import Data.Default

import Brick.Core (SetSize(..), Location(..))
import Brick.Prim (Prim(..))

data HScroll =
    HScroll { scrollLeft :: !Int
            , scrollWidth :: !Int
            }

data VScroll =
    VScroll { scrollTop :: !Int
            , scrollHeight :: !Int
            }

instance SetSize VScroll where
    setSize (_, h) vs = vs { scrollHeight = h }

instance SetSize HScroll where
    setSize (w, _) hs = hs { scrollWidth = w }

instance Default HScroll where
    def = HScroll 0 0

instance Default VScroll where
    def = VScroll 0 0

class Scrollable a where
    setScrollStart :: Int -> a -> a
    scrollStart :: a -> Int
    scrollSize :: a -> Int

instance Scrollable HScroll where
    setScrollStart col hs = hs { scrollLeft = col }
    scrollStart = scrollLeft
    scrollSize = scrollWidth

instance Scrollable VScroll where
    setScrollStart row vs = vs { scrollTop = row }
    scrollStart = scrollTop
    scrollSize = scrollHeight

vScroll :: VScroll -> Prim a -> Prim a
vScroll vs p =
    Translate (Location (0, -1 * scrollStart vs)) $ VRelease p

hScroll :: HScroll -> Prim a -> Prim a
hScroll hs p =
    Translate (Location (-1 * scrollStart hs, 0)) $ HRelease p

scrollToView :: (Scrollable a) => (Int, Int) -> a -> a
scrollToView (reqStart, reqSize) s =
    setScrollStart newStart s
    where
        curEnd = curStart + scrollSize s
        curStart = scrollStart s
        reqEnd = reqStart + reqSize
        newStart = if reqStart < curStart
                   then reqStart
                   else if reqStart > curEnd || reqEnd > curEnd
                        then reqEnd - scrollSize s
                        else curStart
