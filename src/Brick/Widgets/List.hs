{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable#-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module provides a scrollable list type and functions for
-- manipulating and rendering it.
module Brick.Widgets.List
  ( GenericList
  , List

  -- * Constructing a list
  , list

  -- * Rendering a list
  , renderList
  , renderListWithIndex

  -- * Handling events
  , handleListEvent
  , handleListEventVi

  -- * Lenses
  , listElementsL
  , listSelectedL
  , listNameL
  , listItemHeightL

  -- * Accessors
  , listElements
  , listName
  , listSelectedElement
  , listSelected
  , listItemHeight

  -- * Manipulating a list
  , listMoveBy
  , listMoveTo
  , listMoveToElement
  , listMoveUp
  , listMoveDown
  , listMoveByPages
  , listMovePageUp
  , listMovePageDown
  , listInsert
  , listRemove
  , listReplace
  , listClear
  , listReverse
  , listModify

  -- * Attributes
  , listAttr
  , listSelectedAttr
  , listSelectedFocusedAttr

  -- * Classes
  , Splittable(..)
  )
where

import Prelude hiding (splitAt)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*>),pure)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif
import Control.Applicative ((<|>))
import Control.Monad.Trans.State (evalState, get, put)

import Lens.Micro ((^.), (^?), (&), (.~), (%~), _2, _head)
import Data.Functor (($>))
import Data.Foldable (find, toList)
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
import Data.Semigroup (Semigroup, (<>), sconcat)
import qualified Data.Sequence as Seq
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import qualified Data.Vector as V
import GHC.Generics (Generic)

import Brick.Types
import Brick.Main (lookupViewport)
import Brick.Widgets.Core
import Brick.Util (clamp)
import Brick.AttrMap

-- | List state. Lists have a container @t@ of element type @e@ that
-- is the data stored by the list.  Internally, Lists handle the
-- following events by default:
--
-- * Up/down arrow keys: move cursor of selected item
-- * Page up / page down keys: move cursor of selected item by one page
--   at a time (based on the number of items shown)
-- * Home/end keys: move cursor of selected item to beginning or end of
--   list
--
-- The 'List' type synonym fixes @t@ to 'V.Vector'.
--
-- For a container type to be usable with 'GenericList', it must
-- have instances of 'Traversable' and 'Splittable'.  The following
-- functions impose further constraints:
--
-- * 'listInsert': 'Applicative' and 'Semigroup'
-- * 'listRemove': 'Semigroup'
-- * 'listClear': 'Monoid'
--
data GenericList n t e =
    List { listElements :: !(t e)
         -- ^ The list's vector of elements.
         , listSelected :: !(Maybe Int)
         -- ^ The list's selected element index, if any.
         , listName :: n
         -- ^ The list's name.
         , listItemHeight :: Int
         -- ^ The height of the list items.
         } deriving (Functor, Foldable, Traversable, Show, Generic)

suffixLenses ''GenericList

-- | A 'Vector'-based list state
type List n e = GenericList n V.Vector e

instance Named (GenericList n t e) n where
    getName = listName

-- | Ordered container types that can be split at a given index.
-- An instance of this class is required for a container type to be
-- usable with 'GenericList'.
class Splittable t where
  {-# MINIMAL splitAt #-}

  -- | Split at the given index.  Equivalent to @(take n xs, drop n xs)@,
  -- therefore total.
  splitAt :: Int -> t a -> (t a, t a)

  -- | Slice the structure.  Equivalent to @(take n . drop i) xs@,
  -- therefore total.
  --
  -- The default implementation applies 'splitAt' two times: first
  -- to drop elements leading up to the slice, and again to drop
  -- elements after the slice.
  --
  slice :: Int {- ^ start index -} -> Int {- ^ length -} -> t a -> t a
  slice i n = fst . splitAt n . snd . splitAt i

-- | /O(1)/ 'splitAt'.
instance Splittable V.Vector where
  splitAt = V.splitAt

-- | /O(log(min(i,n-i)))/ 'splitAt'.
instance Splittable Seq.Seq where
  splitAt = Seq.splitAt


handleListEvent
  :: (Foldable t, Splittable t, Ord n)
  => Event -> GenericList n t e -> EventM n (GenericList n t e)
handleListEvent e theList =
    case e of
        EvKey KUp [] -> return $ listMoveUp theList
        EvKey KDown [] -> return $ listMoveDown theList
        EvKey KHome [] -> return $ listMoveTo 0 theList
        EvKey KEnd [] -> return $ listMoveTo (length $ listElements theList) theList
        EvKey KPageDown [] -> listMovePageDown theList
        EvKey KPageUp [] -> listMovePageUp theList
        _ -> return theList

-- | Enable list movement with the vi keys with a fallback if none
-- match. Use (handleListEventVi handleListEvent) in place of
-- handleListEvent to add the vi keys bindings to the standard ones.
-- Movements handled include:
--
-- * Up             (k)
-- * Down           (j)
-- * Page Up        (Ctrl-b)
-- * Page Down      (Ctrl-f)
-- * Half Page Up   (Ctrl-u)
-- * Half Page Down (Ctrl-d)
-- * Top            (g)
-- * Bottom         (G)
handleListEventVi :: (Foldable t, Splittable t, Ord n)
                  => (Event -> GenericList n t e
                        -> EventM n (GenericList n t e))
                  -- ^ Fallback event handler to use if none of the vi keys
                  -- match.
                  -> Event
                  -> GenericList n t e
                  -> EventM n (GenericList n t e)
handleListEventVi fallback e theList =
    case e of
        EvKey (KChar 'k') [] -> return $ listMoveUp theList
        EvKey (KChar 'j') [] -> return $ listMoveDown theList
        EvKey (KChar 'g') [] -> return $ listMoveTo 0 theList
        EvKey (KChar 'G') [] -> return $ listMoveTo (length $ listElements theList) theList
        EvKey (KChar 'f') [MCtrl] -> listMovePageDown theList
        EvKey (KChar 'b') [MCtrl] -> listMovePageUp theList
        EvKey (KChar 'd') [MCtrl] -> listMoveByPages (0.5::Double) theList
        EvKey (KChar 'u') [MCtrl] -> listMoveByPages (-0.5::Double) theList
        _ -> fallback e theList

-- | The top-level attribute used for the entire list.
listAttr :: AttrName
listAttr = "list"

-- | The attribute used only for the currently-selected list item when
-- the list does not have focus. Extends 'listAttr'.
listSelectedAttr :: AttrName
listSelectedAttr = listAttr <> "selected"

-- | The attribute used only for the currently-selected list item when
-- the list has focus. Extends 'listSelectedAttr'.
listSelectedFocusedAttr :: AttrName
listSelectedFocusedAttr = listSelectedAttr <> "focused"

-- | Construct a list in terms of container 't' with element type 'e'.
list :: (Foldable t)
     => n
     -- ^ The list name (must be unique)
     -> t e
     -- ^ The initial list contents
     -> Int
     -- ^ The list item height in rows (all list item widgets must be
     -- this high)
     -> GenericList n t e
list name es h =
    let selIndex = if null es then Nothing else Just 0
        safeHeight = max 1 h
    in List es selIndex name safeHeight

-- | Turn a list state value into a widget given an item drawing
-- function.
renderList :: (Traversable t, Splittable t, Ord n, Show n)
           => (Bool -> e -> Widget n)
           -- ^ Rendering function, True for the selected element
           -> Bool
           -- ^ Whether the list has focus
           -> GenericList n t e
           -- ^ The List to be rendered
           -> Widget n
           -- ^ rendered widget
renderList drawElem = renderListWithIndex $ const drawElem

-- | Like 'renderList', except the render function is also provided
-- with the index of each element.
renderListWithIndex :: (Traversable t, Splittable t, Ord n, Show n)
           => (Int -> Bool -> e -> Widget n)
           -- ^ Rendering function, taking index, and True for the
           -- selected element
           -> Bool
           -- ^ Whether the list has focus
           -> GenericList n t e
           -- ^ The List to be rendered
           -> Widget n
           -- ^ rendered widget
renderListWithIndex drawElem foc l =
    withDefAttr listAttr $
    drawListElements foc l drawElem


imap :: (Traversable t) => (Int -> a -> b) -> t a -> t b
imap f xs =
  evalState (traverse (\a -> get >>= \i -> put (i + 1) $> f i a) xs) 0


-- | Draws the list elements.
--
-- Evaluates the underlying container up to, and a bit beyond, the
-- selected element.  The exact amount depends on available height
-- for drawing and 'listItemHeight'.  At most, it will evaluate up
-- to element @(i + h + 1)@ where @i@ is the selected index and @h@
-- is the available height.
--
drawListElements
  :: (Traversable t, Splittable t, Ord n, Show n)
  => Bool
  -> GenericList n t e
  -> (Int -> Bool -> e -> Widget n)
  -> Widget n
drawListElements foc l drawElem =
    Widget Greedy Greedy $ do
        c <- getContext

        let
            -- Take (numPerHeight * 2) elements, or whatever is left
            es = slice start (numPerHeight * 2) (l^.listElementsL)

            idx = fromMaybe 0 (l^.listSelectedL)

            start = max 0 $ idx - numPerHeight + 1

            -- The number of items to show is the available height divided by
            -- the item height...
            initialNumPerHeight = (c^.availHeightL) `div` (l^.listItemHeightL)
            -- ... but if the available height leaves a remainder of
            -- an item height then we need to ensure that we render an
            -- extra item to show a partial item at the top or bottom to
            -- give the expected result when an item is more than one
            -- row high. (Example: 5 rows available with item height
            -- of 3 yields two items: one fully rendered, the other
            -- rendered with only its top 2 or bottom 2 rows visible,
            -- depending on how the viewport state changes.)
            numPerHeight = initialNumPerHeight +
                           if initialNumPerHeight * (l^.listItemHeightL) == c^.availHeightL
                           then 0
                           else 1

            off = start * (l^.listItemHeightL)

            drawnElements = flip imap es $ \i e ->
                let j = i + start
                    isSelected = Just j == l^.listSelectedL
                    elemWidget = drawElem j isSelected e
                    selItemAttr = if foc
                                  then withDefAttr listSelectedFocusedAttr
                                  else withDefAttr listSelectedAttr
                    makeVisible = if isSelected
                                  then visible . selItemAttr
                                  else id
                in makeVisible elemWidget

        render $ viewport (l^.listNameL) Vertical $
                 translateBy (Location (0, off)) $
                 vBox $ toList drawnElements

-- | Insert an item into a list at the specified position.
--
listInsert :: (Splittable t, Applicative t, Semigroup (t e))
           => Int
           -- ^ The position at which to insert (0 <= i <= size)
           -> e
           -- ^ The element to insert
           -> GenericList n t e
           -> GenericList n t e
listInsert pos e l =
    let es = l^.listElementsL
        newSel = case l^.listSelectedL of
          Nothing -> 0
          Just s -> if pos <= s
                    then s + 1
                    else s
        (front, back) = splitAt pos es
    in l & listSelectedL .~ Just newSel
         & listElementsL .~ sconcat (front :| [pure e, back])

-- | Remove an element from a list at the specified position.
--
-- Applies 'splitAt' two times: first to split the structure at the
-- given position, and again to remove the first element from the
-- tail.  Consider the asymptotics of `splitAt` for the container
-- type when using this function.  ('Data.Vector.splitAt' is *O(1)*).
--
listRemove :: (Splittable t, Foldable t, Semigroup (t e))
           => Int
           -- ^ The position at which to remove an element (0 <= i < size)
           -> GenericList n t e
           -> GenericList n t e
listRemove pos l | null (l^.listElementsL) = l
                 | pos /= splitClamp l pos = l
                 | otherwise =
    let newSel = case l^.listSelectedL of
          Nothing -> 0
          Just s | pos == 0 -> 0
                 | pos == s -> pos - 1
                 | pos  < s -> s - 1
                 | otherwise -> s
        (front, rest) = splitAt pos es
        (_, back) = splitAt 1 rest
        es' = front <> back
        es = l^.listElementsL
    in l & listSelectedL .~ (if null es' then Nothing else Just newSel)
         & listElementsL .~ es'

-- | Replace the contents of a list with a new set of elements and
-- update the new selected index. If the list is empty, empty selection is used
-- instead. Otherwise, if the specified selected index (via 'Just') is not in
-- the list bounds, zero is used instead.
--
listReplace
  :: (Foldable t, Splittable t)
  => t e -> Maybe Int -> GenericList n t e -> GenericList n t e
listReplace es idx l =
    let
      l' = l & listElementsL .~ es
      newSel = if null es then Nothing else inBoundsOrZero <$> idx
      inBoundsOrZero i
        | i == splitClamp l' i = i
        | otherwise = 0
    in l' & listSelectedL .~ newSel

-- | Move the list selected index up by one. (Moves the cursor up,
-- subtracts one from the index.)
listMoveUp
  :: (Foldable t, Splittable t)
  => GenericList n t e -> GenericList n t e
listMoveUp = listMoveBy (-1)

-- | Move the list selected index up by one page.
listMovePageUp
  :: (Foldable t, Splittable t, Ord n)
  => GenericList n t e -> EventM n (GenericList n t e)
listMovePageUp theList = listMoveByPages (-1::Double) theList

-- | Move the list selected index down by one. (Moves the cursor down,
-- adds one to the index.)
listMoveDown
  :: (Foldable t, Splittable t)
  => GenericList n t e -> GenericList n t e
listMoveDown = listMoveBy 1

-- | Move the list selected index down by one page.
listMovePageDown
  :: (Foldable t, Splittable t, Ord n)
  => GenericList n t e -> EventM n (GenericList n t e)
listMovePageDown theList = listMoveByPages (1::Double) theList

-- | Move the list selected index by some (fractional) number of pages.
listMoveByPages
  :: (Foldable t, Splittable t, Ord n, RealFrac m)
  => m -> GenericList n t e -> EventM n (GenericList n t e)
listMoveByPages pages theList = do
    v <- lookupViewport (theList^.listNameL)
    case v of
        Nothing -> return theList
        Just vp -> let
            nElems = round $ pages * (fromIntegral $ vp^.vpSize._2) / (fromIntegral $ theList^.listItemHeightL)
          in
            return $ listMoveBy nElems theList

-- | Move the list selected index. If the index is `Just x`, adjust by the
-- specified amount; if it is `Nothing` (i.e. there is no selection) and the
-- direction is positive, set to `Just 0` (first element), otherwise set to
-- `Just (length - 1)` (last element). Subject to validation.
listMoveBy
  :: (Foldable t, Splittable t)
  => Int -> GenericList n t e -> GenericList n t e
listMoveBy amt l =
    let len = length (l^.listElementsL)
        newSel = case l^.listSelectedL of
          Nothing
            | amt > 0 -> 0
            | otherwise -> len - 1
          Just i -> splitClamp l (amt + i)
    in
      l & listSelectedL .~ (if len > 0 then Just newSel else Nothing)

-- | Set the selected index for a list to the specified index, subject
-- to validation.
--
-- If @pos >= 0@, indexes from the start of the list (which gets
-- evaluated up to the target index)
--
-- If @pos < 0@, indexes from the end of the list (which evalutes
-- 'length' of the list).
--
listMoveTo
  :: (Foldable t, Splittable t)
  => Int -> GenericList n t e -> GenericList n t e
listMoveTo pos l =
  let
    len = length (l ^. listElementsL)
    i = if pos < 0 then len - pos else pos
    newSel = splitClamp l i
  in l & listSelectedL .~ if not (null (l ^. listElementsL))
                            then Just newSel
                            else Nothing

-- | Split-based clamp that avoids evaluating 'length' of the
-- structure (unless the structure is already fully evaluated).
--
splitClamp :: (Foldable t, Splittable t) => GenericList n t e -> Int -> Int
splitClamp l i =
  let
    (_, t) = splitAt i (l ^. listElementsL)  -- split at i
  in
    -- If the tail is empty, then the requested index is not in the list.
    -- And because we have already seen the end of the list, using 'length'
    -- will not force unwanted computation.
    --
    -- Otherwise if tail is not empty, then we already know that i
    -- is in the list, so we don't need to know the length
    clamp 0 (if null t then length (l ^. listElementsL) - 1 else i) i

-- | Set the selected index for a list to the index of the specified
-- element if it is in the list, or leave the list unmodified otherwise.
listMoveToElement
  :: (Eq e, Traversable t)
  => e -> GenericList n t e -> GenericList n t e
listMoveToElement e l =
    let i = fmap fst $ find ((== e) . snd) $ imap (,) (l^.listElementsL)
    in l & listSelectedL %~ (i <|>)

-- | Return a list's selected element, if any.
--
-- Only evaluates as much of the container as needed.
--
-- Complexity: same as 'splitAt' for the container type.
--
listSelectedElement
  :: (Splittable t, Foldable t)
  => GenericList n t e -> Maybe (Int, e)
listSelectedElement l = do
  sel <- l^.listSelectedL
  let (_, xs) = splitAt sel (l ^. listElementsL)
  (sel,) <$> toList xs ^? _head

-- | Remove all elements from the list and clear the selection.
listClear :: (Monoid (t e)) => GenericList n t e -> GenericList n t e
listClear l = l & listElementsL .~ mempty & listSelectedL .~ Nothing

-- | Reverse the list.  The element selected before the reversal will
-- again be the selected one.
--
-- For now, this is only implemented for 'Vector'-based lists.
--
listReverse :: List n e -> List n e
listReverse theList = theList & listElementsL %~ V.reverse & listSelectedL .~ newSel
  where n = length (listElements theList)
        newSel = (-) <$> pure (n-1) <*> listSelected theList

-- | Apply a function to the selected element. If no element is selected
-- the list is not modified.
--
-- /O(n)/
--
listModify :: (Traversable t) => (e -> e) -> GenericList n t e -> GenericList n t e
listModify f l = case l ^. listSelectedL of
  Nothing -> l
  Just j -> l & listElementsL %~ imap (\i e -> if i == j then f e else e)
