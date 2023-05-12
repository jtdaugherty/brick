-- | This module provides a type and functions for handling focus rings
-- of values.
module Brick.Focus
  ( FocusRing
  , focusRing
  , focusNext
  , focusPrev
  , focusGetCurrent
  , focusSetCurrent
  , focusRingLength
  , focusRingToList
  , focusRingCursor
  , withFocusRing
  , focusRingModify
  )
where

import Lens.Micro ((^.))
import Data.List (find)
import qualified Data.CircularList as C

import Brick.Types
import Brick.Widgets.Core (Named(..))

-- | A focus ring containing a sequence of resource names to focus and a
-- currently-focused name.
newtype FocusRing n = FocusRing (C.CList n)
                    deriving (Show)

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
              -- ^ The widget state value that we need to check for focus.
              -> b
              -- ^ The rest of the computation.
withFocusRing ring f a = f (focusGetCurrent ring == Just (getName a)) a

-- | Get the currently-focused resource name from the ring. If the ring
-- is empty, return 'Nothing'.
focusGetCurrent :: FocusRing n -> Maybe n
focusGetCurrent (FocusRing l) = C.focus l

-- | Set the currently-focused resource name in the ring, provided the
-- name is in the ring. Otherwise return the ring unmodified.
focusSetCurrent :: (Eq n) => n -> FocusRing n -> FocusRing n
focusSetCurrent n r@(FocusRing l) =
    case C.rotateTo n l of
        Nothing -> r
        Just l' -> FocusRing l'

-- | Get the size of the FocusRing.
focusRingLength :: FocusRing n -> Int
focusRingLength (FocusRing l) = C.size l

-- | Return all of the entries in the focus ring, starting with the
-- currently-focused entry and wrapping around the ring.
--
-- For example, if a ring contains A, B, C, and D, and the current entry
-- is B, the result will be [B, C, D, A].
focusRingToList :: FocusRing n -> [n]
focusRingToList (FocusRing l) = C.rightElements l

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
focusRingCursor getRing st = find $ \cl ->
    cl^.cursorLocationNameL == focusGetCurrent (getRing st)
