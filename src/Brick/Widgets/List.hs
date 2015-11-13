{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module provides a scrollable list type and functions for
-- manipulating and rendering it.
module Brick.Widgets.List
  ( List(listElements, listSelected, listName, listItemHeight)

  -- * Consructing a list
  , list

  -- * Rendering a list
  , renderList

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

  -- * Attributes
  , listAttr
  , listSelectedAttr
  )
where

import Control.Applicative ((<$>))
import Control.Lens ((^.), (&), (.~), _2)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import qualified Data.Algorithm.Diff as D
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
data List e =
    List { listElements :: !(V.Vector e)
         , listSelected :: !(Maybe Int)
         , listName :: Name
         , listItemHeight :: Int
         }

suffixLenses ''List

instance HandleEvent (List e) where
    handleEvent e theList = f
        where
            f = case e of
                  EvKey KUp [] -> return $ listMoveUp theList
                  EvKey KDown [] -> return $ listMoveDown theList
                  EvKey KHome [] -> return $ listMoveTo 0 theList
                  EvKey KEnd [] -> return $ listMoveTo (V.length $ listElements theList) theList
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

-- | The attribute used only for the currently-selected list item.
-- Extends 'listAttr'.
listSelectedAttr :: AttrName
listSelectedAttr = listAttr <> "selected"

-- | Construct a list in terms of an element type 'e'.
list :: Name
     -- ^ The list name (must be unique)
     -> V.Vector e
     -- ^ The initial list contents
     -> Int
     -- ^ The list item height in rows (all list item widgets must be
     -- this high)
     -> List e
list name es h =
    let selIndex = if V.null es then Nothing else Just 0
    in List es selIndex name h

-- | Turn a list state value into a widget given an item drawing
-- function.
renderList :: List e -> (Bool -> e -> Widget) -> Widget
renderList l drawElem =
    withDefAttr listAttr $
    drawListElements l drawElem

drawListElements :: List e -> (Bool -> e -> Widget) -> Widget
drawListElements l drawElem =
    Widget Greedy Greedy $ do
        c <- getContext

        let es = V.slice start num (l^.listElementsL)
            idx = case l^.listSelectedL of
                Nothing -> 0
                Just i -> i

            start = max 0 $ idx - numPerHeight + 1
            num = min (numPerHeight * 2) (V.length (l^.listElementsL) - start)
            numPerHeight = (c^.availHeightL) `div` (l^.listItemHeightL)

            off = start * (l^.listItemHeightL)

            drawnElements = (flip V.imap) es $ \i e ->
                let isSelected = Just (i + start) == l^.listSelectedL
                    elemWidget = drawElem isSelected e
                    makeVisible = if isSelected
                                  then (visible . withDefAttr listSelectedAttr)
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
           -> List e
           -> List e
listInsert pos e l =
    let safePos = clamp 0 (V.length es) pos
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
           -> List e
           -> List e
listRemove pos l | V.null (l^.listElementsL) = l
                 | pos /= clamp 0 (V.length (l^.listElementsL) - 1) pos = l
                 | otherwise =
    let newSel = case l^.listSelectedL of
          Nothing -> 0
          Just s  -> if pos == 0
                     then 0
                     else if pos == s
                          then pos - 1
                          else if pos < s
                               then s - 1
                               else s
        (front, back) = V.splitAt pos es
        es' = front V.++ V.tail back
        es = l^.listElementsL
    in l & listSelectedL .~ (if V.null es' then Nothing else Just newSel)
         & listElementsL .~ es'

-- | Replace the contents of a list with a new set of elements but
-- preserve the currently selected index.
listReplace :: Eq e => V.Vector e -> List e -> List e
listReplace es' l | es' == l^.listElementsL = l
                  | otherwise =
    let sel = fromMaybe 0 (l^.listSelectedL)
        getNewSel es = case (V.null es, V.null es') of
          (_, True)      -> Nothing
          (True, False)  -> Just 0
          (False, False) -> Just (maintainSel (V.toList es) (V.toList es') sel)
        newSel = getNewSel (l^.listElementsL)

    in l & listSelectedL .~ newSel
         & listElementsL .~ es'

-- | Move the list selected index up by one. (Moves the cursor up,
-- subtracts one from the index.)
listMoveUp :: List e -> List e
listMoveUp = listMoveBy (-1)

-- | Move the list selected index down by one. (Moves the cursor down,
-- adds one to the index.)
listMoveDown :: List e -> List e
listMoveDown = listMoveBy 1

-- | Move the list selected index by the specified amount, subject to
-- validation.
listMoveBy :: Int -> List e -> List e
listMoveBy amt l =
    let newSel = clamp 0 (V.length (l^.listElementsL) - 1) <$> (amt +) <$> (l^.listSelectedL)
    in l & listSelectedL .~ newSel

-- | Set the selected index for a list to the specified index, subject
-- to validation.
listMoveTo :: Int -> List e -> List e
listMoveTo pos l =
    let len = V.length (l^.listElementsL)
        newSel = clamp 0 (len - 1) $ if pos < 0 then (len - pos) else pos
    in l & listSelectedL .~ if len > 0
                            then Just newSel
                            else Nothing

-- | Return a list's selected element, if any.
listSelectedElement :: List e -> Maybe (Int, e)
listSelectedElement l = do
  sel <- l^.listSelectedL
  return (sel, (l^.listElementsL) V.! sel)

-- Assuming `xs` is an existing list that we want to update to match the
-- state of `ys`. Given a selected index in `xs`, the goal is to compute
-- the corresponding index in `ys`.
maintainSel :: (Eq e) => [e] -> [e] -> Int -> Int
maintainSel xs ys sel = let hunks = D.getDiff xs ys
                        in clamp 0 (length ys - 1) $ merge 0 sel hunks

-- Given (0, sel, diff), computes the value 'sel' would have after 'diff'
-- was applied. Retuns -1 if the selected item doesn't appear anywhere in
-- the replacement list.
merge :: (Eq e) => Int -> Int -> [D.Diff e] -> Int
merge _   sel []                 = sel
merge idx sel (h:hs) | idx > sel = sel
                     | otherwise = case h of
    D.Both _ _ -> merge (idx + 1) sel hs

    -- element removed in new list
    D.First _  -> merge idx (sel - 1) hs

    -- element added in new list
    D.Second _ -> let newSel = if idx <= sel
                               then sel + 1
                               else sel
                  in merge (idx + 1) newSel hs
