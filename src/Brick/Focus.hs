-- | This module provides a type and functions for handling focus rings
-- of values.
--
-- This interface is experimental.
module Brick.Focus
  ( FocusRing
  , focusRing
  , focusNext
  , focusPrev
  , focusGetCurrent
  , focusRingCursor
  , withFocusRing
  , focusRingModify
  )
where

import Lens.Micro ((^.))
import Data.Maybe (listToMaybe)
import qualified Data.CircularList as C

import Brick.Types
import Brick.Widgets.Core (Named(..))

-- | A focus ring containing a sequence of resource names to focus and a
-- currently-focused name.
newtype FocusRing n = FocusRing (C.CList n)

-- | Construct a focus ring from the list of resource names.
focusRing :: [n] -> FocusRing n
focusRing = FocusRing . C.fromList

-- | Advance focus to the next value in the ring.
focusNext :: FocusRing n -> FocusRing n
focusNext r@(FocusRing l)
    | C.isEmpty l = r
    | otherwise = FocusRing $ C.rotR l

-- | Advance focus to the previous value in the ring.
focusPrev :: FocusRing n -> FocusRing n
focusPrev r@(FocusRing l)
    | C.isEmpty l = r
    | otherwise = FocusRing $ C.rotL l

-- | This function is a convenience function to look up a widget state
-- value's resource name in a focus ring and set its focus setting
-- according to the focus ring's state. This function determines whether
-- a given widget state value is the focus of the ring and passes the
-- resulting boolean to a rendering function, along with the state value
-- (a), to produce whatever comes next (b).
--
-- Focus-aware widgets have rendering functions that should be
-- usable with this combinator; see 'Brick.Widgets.List.List' and
-- 'Brick.Widgets.Edit.Edit'.
withFocusRing :: (Eq n, Named a n)
              => FocusRing n
              -- ^ The focus ring to use as the source of focus state.
              -> (Bool -> a -> b)
              -- ^ A function that takes a value and its focus state.
              -> a
              -- ^ The wiget state value that we need to check for focus.
              -> b
              -- ^ The rest of the computation.
withFocusRing ring f a = f (focusGetCurrent ring == Just (getName a)) a

-- | Get the currently-focused resource name from the ring. If the ring
-- is emtpy, return 'Nothing'.
focusGetCurrent :: FocusRing n -> Maybe n
focusGetCurrent (FocusRing l) = C.focus l

-- | Modify the internal circular list structure of a focus ring
-- directly. This function permits modification of the circular list
-- using the rich Data.CircularList API.
focusRingModify :: (C.CList n -> C.CList n) -> FocusRing n -> FocusRing n
focusRingModify f (FocusRing l) = FocusRing $ f l

-- | Cursor selection convenience function for use as an
-- 'Brick.Main.appChooseCursor' value.
focusRingCursor :: (Eq n)
                => (a -> FocusRing n)
                -- ^ The function used to get the focus ring out of your
                -- application state.
                -> a
                -- ^ Your application state.
                -> [CursorLocation n]
                -- ^ The list of available cursor positions.
                -> Maybe (CursorLocation n)
                -- ^ The cursor position, if any, that matches the
                -- resource name currently focused by the 'FocusRing'.
focusRingCursor getRing st ls =
    listToMaybe $ filter isCurrent ls
    where
        isCurrent cl = cl^.cursorLocationNameL ==
                       (focusGetCurrent $ getRing st)
