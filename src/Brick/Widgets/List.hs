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
--
-- Note that lenses are provided for direct manipulation purposes, but
-- lenses are *not* safe and should be used with care. (For example,
-- 'listElementsL' permits direct manipulation of the list container
-- without performing bounds checking on the selected index.) If you
-- need a safe API, consider one of the various functions for list
-- manipulation. For example, instead of 'listElementsL', consider
-- 'listReplace'.
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
  , listFindBy
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
  , Reversible(..)
  )
where

import Prelude hiding (reverse, splitAt, filter)

import Control.Applicative ((<|>))
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
import Data.Foldable (Foldable, find, toList)
import Data.Traversable (Traversable)
#else
import Data.Foldable (find, toList)
#endif
import Control.Monad.Trans.State (evalState, get, put)

import Lens.Micro ((^.), (^?), (&), (.~), (%~), _2, _head, set)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (fromMaybe)
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup, (<>))
#endif
import Data.Semigroup (sconcat)
import qualified Data.Sequence as Seq
import Graphics.Vty (Event(..), Key(..), Modifier(..))
import qualified Data.Vector as V
import GHC.Generics (Generic)
import qualified Data.Witherable.Class as W

import Brick.Types
import Brick.Main (lookupViewport)
import Brick.Widgets.Core
import Brick.Util (clamp)
import Brick.AttrMap

-- | List state. Lists have a container @t@ of element type @e@ that is
-- the data stored by the list. Internally, Lists handle the following
-- events by default:
--
-- * Up/down arrow keys: move cursor of selected item
-- * Page up / page down keys: move cursor of selected item by one page
--   at a time (based on the number of items shown)
-- * Home/end keys: move cursor of selected item to beginning or end of
--   list
--
-- The 'List' type synonym fixes @t@ to 'V.Vector' for compatibility
-- with previous versions of this library.
--
-- For a container type to be usable with 'GenericList', it must have
-- instances of 'Traversable' and 'Splittable'. The following functions
-- impose further constraints:
--
-- * 'listInsert': 'Applicative' and 'Semigroup'
-- * 'listRemove': 'Semigroup'
-- * 'listClear': 'Monoid'
-- * 'listReverse': 'Reversible'
--
data GenericList n t e =
    List { listElements :: !(t e)
         -- ^ The list's sequence of elements.
         , listSelected :: !(Maybe Int)
         -- ^ The list's selected element index, if any.
         , listName :: n
         -- ^ The list's name.
         , listItemHeight :: Int
         -- ^ The height of an individual item in the list.
         } deriving (Functor, Foldable, Traversable, Show, Generic)

suffixLenses ''GenericList

-- | An alias for 'GenericList' specialized to use a 'Vector' as its
-- container type.
type List n e = GenericList n V.Vector e

instance Named (GenericList n t e) n where
    getName = listName

instance W.Filterable t => W.Filterable (GenericList n t) where
  catMaybes l = l & listElementsL %~ W.catMaybes

instance (Traversable t, W.Filterable t) => W.Witherable (GenericList n t) where

-- | Ordered container types that can be split at a given index. An
-- instance of this class is required for a container type to be usable
-- with 'GenericList'.
class Splittable t where
    {-# MINIMAL splitAt #-}

    -- | Split at the given index. Equivalent to @(take n xs, drop n xs)@
    -- and therefore total.
    splitAt :: Int -> t a -> (t a, t a)

    -- | Slice the structure. Equivalent to @(take n . drop i) xs@ and
    -- therefore total.
    --
    -- The default implementation applies 'splitAt' two times: first to
    -- drop elements leading up to the slice, and again to drop elements
    -- after the slice.
    slice :: Int {- ^ start index -} -> Int {- ^ length -} -> t a -> t a
    slice i n = fst . splitAt n . snd . splitAt i

-- | /O(1)/ 'splitAt'.
instance Splittable V.Vector where
    splitAt = V.splitAt

-- | /O(log(min(i,n-i)))/ 'splitAt'.
instance Splittable Seq.Seq where
    splitAt = Seq.splitAt

-- | Ordered container types where the order of elements can be
-- reversed. Only required if you want to use 'listReverse'.
class Reversible t where
    {-# MINIMAL reverse #-}
    reverse :: t a -> t a

-- | /O(n)/ 'reverse'
instance Reversible V.Vector where
  reverse = V.reverse

-- | /O(n)/ 'reverse'
instance Reversible Seq.Seq where
  reverse = Seq.reverse

-- | Handle events for list cursor movement.  Events handled are:
--
-- * Up (up arrow key)
-- * Down (down arrow key)
-- * Page Up (PgUp)
-- * Page Down (PgDown)
-- * Go to first element (Home)
-- * Go to last element (End)
handleListEvent :: (Foldable t, Splittable t, Ord n)
                => Event
                -> GenericList n t e
                -> EventM n (GenericList n t e)
handleListEvent e theList =
    case e of
        EvKey KUp [] -> return $ listMoveUp theList
        EvKey KDown [] -> return $ listMoveDown theList
        EvKey KHome [] -> return $ listMoveTo 0 theList
        EvKey KEnd [] -> return $ listMoveTo (length $ listElements theList) theList
        EvKey KPageDown [] -> listMovePageDown theList
        EvKey KPageUp [] -> listMovePageUp theList
        _ -> return theList

-- | Enable list movement with the vi keys with a fallback handler if
-- none match. Use 'handleListEventVi' 'handleListEvent' in place of
-- 'handleListEvent' to add the vi keys bindings to the standard ones.
-- Movements handled include:
--
-- * Up (k)
-- * Down (j)
-- * Page Up (Ctrl-b)
-- * Page Down (Ctrl-f)
-- * Half Page Up (Ctrl-u)
-- * Half Page Down (Ctrl-d)
-- * Go to first element (g)
-- * Go to last element (G)
handleListEventVi :: (Foldable t, Splittable t, Ord n)
                  => (Event -> GenericList n t e -> EventM n (GenericList n t e))
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
     -- this high).
     -> GenericList n t e
list name es h =
    let selIndex = if null es then Nothing else Just 0
        safeHeight = max 1 h
    in List es selIndex name safeHeight

-- | Render a list using the specified item drawing function.
--
-- Evaluates the underlying container up to, and a bit beyond, the
-- selected element. The exact amount depends on available height
-- for drawing and 'listItemHeight'. At most, it will evaluate up to
-- element @(i + h + 1)@ where @i@ is the selected index and @h@ is the
-- available height.
--
-- Note that this function renders the list with the 'listAttr' as
-- the default attribute and then uses 'listSelectedAttr' as the
-- default attribute for the selected item if the list is not focused
-- or 'listSelectedFocusedAttr' otherwise. This is provided as a
-- convenience so that the item rendering function doesn't have to be
-- concerned with attributes, but if those attributes are undesirable
-- for your purposes, 'forceAttr' can always be used by the item
-- rendering function to ensure that another attribute is used instead.
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

-- | Like 'renderList', except the render function is also provided with
-- the index of each element.
--
-- Has the same evaluation characteristics as 'renderList'.
renderListWithIndex :: (Traversable t, Splittable t, Ord n, Show n)
                    => (Int -> Bool -> e -> Widget n)
                    -- ^ Rendering function, taking index, and True for
                    -- the selected element
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
    let act = traverse (\a -> get >>= \i -> put (i + 1) $> f i a) xs
    in evalState act 0

-- | Draws the list elements.
--
-- Evaluates the underlying container up to, and a bit beyond, the
-- selected element. The exact amount depends on available height
-- for drawing and 'listItemHeight'. At most, it will evaluate up to
-- element @(i + h + 1)@ where @i@ is the selected index and @h@ is the
-- available height.
drawListElements :: (Traversable t, Splittable t, Ord n, Show n)
                 => Bool
                 -> GenericList n t e
                 -> (Int -> Bool -> e -> Widget n)
                 -> Widget n
drawListElements foc l drawElem =
    Widget Greedy Greedy $ do
        c <- getContext

        -- Take (numPerHeight * 2) elements, or whatever is left
        let es = slice start (numPerHeight * 2) (l^.listElementsL)

            idx = fromMaybe 0 (l^.listSelectedL)

            start = max 0 $ idx - numPerHeight + 1

            -- The number of items to show is the available height
            -- divided by the item height...
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
-- Complexity: the worse of 'splitAt' and `<>` for the container type.
--
-- @
-- listInsert for 'List': O(n)
-- listInsert for 'Seq.Seq': O(log(min(i, length n - i)))
-- @
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
-- given position, and again to remove the first element from the tail.
-- Consider the asymptotics of `splitAt` for the container type when
-- using this function.
--
-- Complexity: the worse of 'splitAt' and `<>` for the container type.
--
-- @
-- listRemove for 'List': O(n)
-- listRemove for 'Seq.Seq': O(log(min(i, n - i)))
-- @
listRemove :: (Splittable t, Foldable t, Semigroup (t e))
           => Int
           -- ^ The position at which to remove an element (0 <= i <
           -- size)
           -> GenericList n t e
           -> GenericList n t e
listRemove pos l | null l = l
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
-- update the new selected index. If the list is empty, empty selection
-- is used instead. Otherwise, if the specified selected index (via
-- 'Just') is not in the list bounds, zero is used instead.
--
-- Complexity: same as 'splitAt' for the container type.
listReplace :: (Foldable t, Splittable t)
            => t e
            -> Maybe Int
            -> GenericList n t e
            -> GenericList n t e
listReplace es idx l =
    let l' = l & listElementsL .~ es
        newSel = if null es then Nothing else inBoundsOrZero <$> idx
        inBoundsOrZero i
            | i == splitClamp l' i = i
            | otherwise = 0
    in l' & listSelectedL .~ newSel

-- | Move the list selected index up by one. (Moves the cursor up,
-- subtracts one from the index.)
listMoveUp :: (Foldable t, Splittable t)
           => GenericList n t e
           -> GenericList n t e
listMoveUp = listMoveBy (-1)

-- | Move the list selected index up by one page.
listMovePageUp :: (Foldable t, Splittable t, Ord n)
               => GenericList n t e
               -> EventM n (GenericList n t e)
listMovePageUp = listMoveByPages (-1::Double)

-- | Move the list selected index down by one. (Moves the cursor down,
-- adds one to the index.)
listMoveDown :: (Foldable t, Splittable t)
             => GenericList n t e
             -> GenericList n t e
listMoveDown = listMoveBy 1

-- | Move the list selected index down by one page.
listMovePageDown :: (Foldable t, Splittable t, Ord n)
                 => GenericList n t e
                 -> EventM n (GenericList n t e)
listMovePageDown = listMoveByPages (1::Double)

-- | Move the list selected index by some (fractional) number of pages.
listMoveByPages :: (Foldable t, Splittable t, Ord n, RealFrac m)
                => m
                -> GenericList n t e
                -> EventM n (GenericList n t e)
listMoveByPages pages theList = do
    v <- lookupViewport (theList^.listNameL)
    case v of
        Nothing -> return theList
        Just vp -> do
            let nElems = round $ pages * fromIntegral (vp^.vpSize._2) /
                                 fromIntegral (theList^.listItemHeightL)
            return $ listMoveBy nElems theList

-- | Move the list selected index.
--
-- If the current selection is @Just x@, the selection is adjusted by
-- the specified amount. The value is clamped to the extents of the list
-- (i.e. the selection does not "wrap").
--
-- If the current selection is @Nothing@ (i.e. there is no selection)
-- and the direction is positive, set to @Just 0@ (first element),
-- otherwise set to @Just (length - 1)@ (last element).
--
-- Complexity: same as 'splitAt' for the container type.
--
-- @
-- listMoveBy for 'List': O(1)
-- listMoveBy for 'Seq.Seq': O(log(min(i,n-i)))
-- @
listMoveBy :: (Foldable t, Splittable t)
           => Int
           -> GenericList n t e
           -> GenericList n t e
listMoveBy amt l =
    let target = case l ^. listSelectedL of
            Nothing
                | amt > 0 -> 0
                | otherwise -> length l - 1
            Just i -> max 0 (amt + i)  -- don't be negative
    in listMoveTo target l

-- | Set the selected index for a list to the specified index, subject
-- to validation.
--
-- If @pos >= 0@, indexes from the start of the list (which gets
-- evaluated up to the target index)
--
-- If @pos < 0@, indexes from the end of the list (which evalutes
-- 'length' of the list).
--
-- Complexity: same as 'splitAt' for the container type.
--
-- @
-- listMoveTo for 'List': O(1)
-- listMoveTo for 'Seq.Seq': O(log(min(i,n-i)))
-- @
listMoveTo :: (Foldable t, Splittable t)
           => Int
           -> GenericList n t e
           -> GenericList n t e
listMoveTo pos l =
    let len = length l
        i = if pos < 0 then len - pos else pos
        newSel = splitClamp l i
    in l & listSelectedL .~ if null l then Nothing else Just newSel

-- | Split-based clamp that avoids evaluating 'length' of the structure
-- (unless the structure is already fully evaluated).
splitClamp :: (Foldable t, Splittable t) => GenericList n t e -> Int -> Int
splitClamp l i =
    let (_, t) = splitAt i (l ^. listElementsL)  -- split at i
    in
        -- If the tail is empty, then the requested index is not in the
        -- list. And because we have already seen the end of the list,
        -- using 'length' will not force unwanted computation.
        --
        -- Otherwise if tail is not empty, then we already know that i
        -- is in the list, so we don't need to know the length
        clamp 0 (if null t then length l - 1 else i) i

-- | Set the selected index for a list to the index of the first
-- occurrence of the specified element if it is in the list, or leave
-- the list unmodified otherwise.
--
-- /O(n)/.  Only evaluates as much of the container as needed.
listMoveToElement :: (Eq e, Foldable t, Splittable t)
                  => e
                  -> GenericList n t e
                  -> GenericList n t e
listMoveToElement e = listFindBy (== e) . set listSelectedL Nothing

-- | Starting from the currently-selected position, attempt to find
-- and select the next element matching the predicate. If there are no
-- matches for the remainder of the list or if the list has no selection
-- at all, the search starts at the beginning. If no matching element is
-- found anywhere in the list, leave the list unmodified.
--
-- /O(n)/.  Only evaluates as much of the container as needed.
listFindBy :: (Foldable t, Splittable t)
           => (e -> Bool)
           -> GenericList n t e
           -> GenericList n t e
listFindBy test l =
    let start = maybe 0 (+1) (l ^. listSelectedL)
        (h, t) = splitAt start (l ^. listElementsL)
        tailResult = find (test . snd) . zip [start..] . toList $ t
        headResult = find (test . snd) . zip [0..] . toList $ h
        result = tailResult <|> headResult
    in maybe id (set listSelectedL . Just . fst) result l

-- | Return a list's selected element, if any.
--
-- Only evaluates as much of the container as needed.
--
-- Complexity: same as 'splitAt' for the container type.
--
-- @
-- listSelectedElement for 'List': O(1)
-- listSelectedElement for 'Seq.Seq': O(log(min(i, n - i)))
-- @
listSelectedElement :: (Splittable t, Foldable t)
                    => GenericList n t e
                    -> Maybe (Int, e)
listSelectedElement l = do
    sel <- l^.listSelectedL
    let (_, xs) = splitAt sel (l ^. listElementsL)
    (sel,) <$> toList xs ^? _head

-- | Remove all elements from the list and clear the selection.
--
-- /O(1)/
listClear :: (Monoid (t e)) => GenericList n t e -> GenericList n t e
listClear l = l & listElementsL .~ mempty & listSelectedL .~ Nothing

-- | Reverse the list. The element selected before the reversal will
-- again be the selected one.
--
-- Complexity: same as 'reverse' for the container type.
--
-- @
-- listReverse for 'List': O(n)
-- listReverse for 'Seq.Seq': O(n)
-- @
listReverse :: (Reversible t, Foldable t)
            => GenericList n t e
            -> GenericList n t e
listReverse l =
    l & listElementsL %~ reverse
      & listSelectedL %~ fmap (length l - 1 -)

-- | Apply a function to the selected element. If no element is selected
-- the list is not modified.
--
-- Complexity: same as 'traverse' for the container type (typically
-- /O(n)/).
listModify :: (Traversable t)
           => (e -> e)
           -> GenericList n t e
           -> GenericList n t e
listModify f l =
    case l ^. listSelectedL of
        Nothing -> l
        Just j -> l & listElementsL %~ imap (\i e -> if i == j then f e else e)
