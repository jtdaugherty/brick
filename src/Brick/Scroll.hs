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
import Brick.Prim

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

vScroll :: (a -> VScroll) -> Render a -> Render a
vScroll f p = do
    result <- vRelease p
    readState $ \s ->
        let vs = f s
        in translate (Location (0, -1 * scrollStart vs)) $ return result

hScroll :: (a -> HScroll) -> Render a -> Render a
hScroll f p = do
    result <- hRelease p
    readState $ \s ->
        let hs = f s
        in translate (Location (-1 * scrollStart hs, 0)) $ return result

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
