{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable#-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- | This module provides a scrollable list type and functions for
-- manipulating and rendering it.
module Brick.Widgets.List
  ( List(listElements, listSelected, listName, listItemHeight)

  -- * Constructing a list
  , list

  -- * Rendering a list
  , renderList

  -- * Handling events
  , handleListEvent

  -- * Lenses
  , listElementsL
  , listSelectedL
  , listNameL
  , listItemHeightL

  -- * Manipulating a list
  , listMoveBy
  , listMoveTo
  , listMoveUp
  , listMoveDown
  , listInsert
  , listRemove
  , listReplace
  , listSelectedElement
  , listClear
  , listReverse

  -- * Attributes
  , listAttr
  , listSelectedAttr
  , listSelectedFocusedAttr
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>),(<*>),pure)
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
#endif

import Lens.Micro ((^.), (&), (.~), (%~), _2)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Graphics.Vty (Event(..), Key(..))
import qualified Data.Vector as V

import Brick.Types
import Brick.Main (lookupViewport)
import Brick.Widgets.Core
import Brick.Util (clamp)
import Brick.AttrMap

-- | List state. Lists have an element type 'e' that is the data stored
-- by the list.  Lists handle the following events by default:
--
-- * Up/down arrow keys: move cursor of selected item
-- * Page up / page down keys: move cursor of selected item by one page
--   at a time (based on the number of items shown)
-- * Home/end keys: move cursor of selected item to beginning or end of
--   list
data List n e =
    List { listElements :: !(V.Vector e)
         , listSelected :: !(Maybe Int)
         , listName :: n
         , listItemHeight :: Int
         } deriving (Functor, Foldable, Traversable, Show)

suffixLenses ''List

instance Named (List n e) n where
    getName = listName

handleListEvent :: (Ord n) => Event -> List n e -> EventM n (List n e)
handleListEvent e theList =
    case e of
        EvKey KUp [] -> return $ listMoveUp theList
        EvKey KDown [] -> return $ listMoveDown theList
        EvKey KHome [] -> return $ listMoveTo 0 theList
        EvKey KEnd [] -> return $ listMoveTo (length theList) theList
        EvKey KPageDown [] -> do
            v <- lookupViewport (theList^.listNameL)
            case v of
                Nothing -> return theList
                Just vp -> return $ listMoveBy (vp^.vpSize._2 `div` theList^.listItemHeightL) theList
        EvKey KPageUp [] -> do
            v <- lookupViewport (theList^.listNameL)
            case v of
                Nothing -> return theList
                Just vp -> return $ listMoveBy (negate $ vp^.vpSize._2 `div` theList^.listItemHeightL) theList
        _ -> return theList

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

-- | Construct a list in terms of an element type 'e'.
list :: n
     -- ^ The list name (must be unique)
     -> V.Vector e
     -- ^ The initial list contents
     -> Int
     -- ^ The list item height in rows (all list item widgets must be
     -- this high)
     -> List n e
list name es h =
    let selIndex = if V.null es then Nothing else Just 0
        safeHeight = max 1 h
    in List es selIndex name safeHeight

-- | Turn a list state value into a widget given an item drawing
-- function.
renderList :: (Ord n, Show n)
           => (Bool -> e -> Widget n)
           -- ^ Rendering function, True for the selected element
           -> Bool
           -- ^ Whether the list has focus
           -> List n e
           -- ^ The List to be rendered
           -> Widget n
           -- ^ rendered widget
renderList drawElem foc l =
    withDefAttr listAttr $
    drawListElements foc l drawElem

drawListElements :: (Ord n, Show n) => Bool -> List n e -> (Bool -> e -> Widget n) -> Widget n
drawListElements foc l drawElem =
    Widget Greedy Greedy $ do
        c <- getContext

        let es = V.slice start num (l^.listElementsL)
            idx = fromMaybe 0 (l^.listSelectedL)

            start = max 0 $ idx - numPerHeight + 1
            num = min (numPerHeight * 2) (length l - start)

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

            drawnElements = flip V.imap es $ \i e ->
                let isSelected = Just (i + start) == l^.listSelectedL
                    elemWidget = drawElem isSelected e
                    selItemAttr = if foc
                                  then withDefAttr listSelectedFocusedAttr
                                  else withDefAttr listSelectedAttr
                    makeVisible = if isSelected
                                  then visible . selItemAttr
                                  else id
                in makeVisible elemWidget

        render $ viewport (l^.listNameL) Vertical $
                 translateBy (Location (0, off)) $
                 vBox $ V.toList drawnElements

-- | Insert an item into a list at the specified position.
listInsert :: Int
           -- ^ The position at which to insert (0 <= i <= size)
           -> e
           -- ^ The element to insert
           -> List n e
           -> List n e
listInsert pos e l =
    let safePos = clamp 0 (length l) pos
        es = l^.listElementsL
        newSel = case l^.listSelectedL of
          Nothing -> 0
          Just s -> if safePos < s
                    then s + 1
                    else s
        (front, back) = V.splitAt safePos es
    in l & listSelectedL .~ Just newSel
         & listElementsL .~ (front V.++ (e `V.cons` back))

-- | Remove an element from a list at the specified position.
listRemove :: Int
           -- ^ The position at which to remove an element (0 <= i < size)
           -> List n e
           -> List n e
listRemove pos l | null l = l
                 | pos /= clamp 0 (length l - 1) pos = l
                 | otherwise =
    let newSel = case l^.listSelectedL of
          Nothing -> 0
          Just s | pos == 0 -> 0
                 | pos == s -> pos - 1
                 | pos  < s -> s - 1
                 | otherwise -> s
        (front, back) = V.splitAt pos es
        es' = front V.++ V.tail back
        es = l^.listElementsL
    in l & listSelectedL .~ (if null l then Nothing else Just newSel)
         & listElementsL .~ es'

-- | Replace the contents of a list with a new set of elements and
-- update the new selected index. If the specified selected index (via
-- 'Just') is not in the list bounds, zero is used instead.
listReplace :: V.Vector e -> Maybe Int -> List n e -> List n e
listReplace es idx l =
    let newSel = clamp 0 (V.length es - 1) <$> idx
    in l & listSelectedL .~ newSel
         & listElementsL .~ es

-- | Move the list selected index up by one. (Moves the cursor up,
-- subtracts one from the index.)
listMoveUp :: List n e -> List n e
listMoveUp = listMoveBy (-1)

-- | Move the list selected index down by one. (Moves the cursor down,
-- adds one to the index.)
listMoveDown :: List n e -> List n e
listMoveDown = listMoveBy 1

-- | Move the list selected index by the specified amount, subject to
-- validation.
listMoveBy :: Int -> List n e -> List n e
listMoveBy amt l =
    let newSel = clamp 0 (length l - 1) <$> (amt +) <$> (l^.listSelectedL)
    in l & listSelectedL .~ newSel

-- | Set the selected index for a list to the specified index, subject
-- to validation.
listMoveTo :: Int -> List n e -> List n e
listMoveTo pos l =
    let len = length l
        newSel = clamp 0 (len - 1) $ if pos < 0 then len - pos else pos
    in l & listSelectedL .~ if len > 0
                            then Just newSel
                            else Nothing

-- | Return a list's selected element, if any.
listSelectedElement :: List n e -> Maybe (Int, e)
listSelectedElement l = do
  sel <- l^.listSelectedL
  return (sel, (l^.listElementsL) V.! sel)

-- | Remove all elements from the list and clear the selection.
listClear :: List n e -> List n e
listClear l = l & listElementsL .~ V.empty & listSelectedL .~ Nothing

-- | Reverse the list.  The element selected before the reversal will
-- again be the selected one.
listReverse :: List n e -> List n e
listReverse theList = theList & listElementsL %~ V.reverse & listSelectedL .~ newSel
  where n = length theList
        newSel = (-) <$> pure (n-1) <*> listSelected theList
