{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module provides a scrollable list type and functions for
-- manipulating and rendering it.
module Brick.Widgets.List
  ( List(listElements, listSelected, listName, listElementDraw)

  -- * Consructing a list
  , list

  -- * Rendering a list
  , renderList

  -- * Lenses
  , listElementsL
  , listSelectedL
  , listNameL

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
import Control.Lens ((^.), (&), (.~))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Algorithm.Diff
import Graphics.Vty (Event(..), Key(..))

import Brick.Types
import Brick.Widgets.Core
import Brick.Util (clamp)
import Brick.AttrMap

-- | List state. Lists have an element type 'e' that is the data stored
-- by the list.
data List e =
    List { listElements :: ![e]
         , listElementDraw :: Bool -> e -> Widget
         , listSelected :: !(Maybe Int)
         , listName :: Name
         }

suffixLenses ''List

instance HandleEvent (List e) where
    handleEvent e theList = f theList
        where
            f = case e of
                  EvKey KUp [] -> listMoveUp
                  EvKey KDown [] -> listMoveDown
                  _ -> id

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
     -> (Bool -> e -> Widget)
     -- ^ The item rendering function (takes the item and whether it is
     -- currently selected)
     -> [e]
     -- ^ The initial list contents
     -> List e
list name draw es =
    let selIndex = if null es then Nothing else Just 0
    in List es draw selIndex name

-- | Turn a list state value into a widget.
renderList :: List e -> Widget
renderList l = withDefaultAttr listAttr $
               viewport (l^.listNameL) Vertical $
               vBox $
               drawListElements l

drawListElements :: List e -> [Widget]
drawListElements l = drawnElements
    where
        es = l^.listElementsL
        drawnElements = (flip map) (zip [0..] es) $ \(i, e) ->
            let isSelected = Just i == l^.listSelectedL
                elemWidget = (l^.listElementDrawL) isSelected e
                makeVisible = if isSelected
                              then (visible . withDefaultAttr listSelectedAttr)
                              else id
            in makeVisible elemWidget

-- | Insert an item into a list at the specified position.
listInsert :: Int
           -- ^ The position at which to insert (0 <= i <= size)
           -> e
           -- ^ The element to insert
           -> List e
           -> List e
listInsert pos e l =
    let safePos = clamp 0 (length es) pos
        es = l^.listElementsL
        newSel = case l^.listSelectedL of
          Nothing -> 0
          Just s -> if safePos < s
                    then s + 1
                    else s
        (front, back) = splitAt safePos es
    in l & listSelectedL .~ Just newSel
         & listElementsL .~ (front ++ (e : back))

-- | Remove an element from a list at the specified position.
listRemove :: Int
           -- ^ The position at which to remove an element (0 <= i < size)
           -> List e
           -> List e
listRemove pos l | null (l^.listElementsL) = l
                 | pos /= clamp 0 (length (l^.listElementsL) - 1) pos = l
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
        (front, back) = splitAt pos es
        es' = front ++ tail back
        es = l^.listElementsL
    in l & listSelectedL .~ (if null es' then Nothing else Just newSel)
         & listElementsL .~ es'

-- | Replace the contents of a list with a new set of elements but
-- preserve the currently selected index.
listReplace :: Eq e => [e] -> List e -> List e
listReplace es' l | es' == l^.listElementsL = l
                  | otherwise =
    let sel = fromMaybe 0 (l^.listSelectedL)
        getNewSel es = case (null es, null es') of
          (_, True)      -> Nothing
          (True, False)  -> Just 0
          (False, False) -> Just (maintainSel es es' sel)
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
    let newSel = clamp 0 (length (l^.listElementsL) - 1) <$> (amt +) <$> (l^.listSelectedL)
    in l & listSelectedL .~ newSel

-- | Set the selected index for a list to the specified index, subject
-- to validation.
listMoveTo :: Int -> List e -> List e
listMoveTo pos l =
    let len = length (l^.listElementsL)
        newSel = clamp 0 (len - 1) $ if pos < 0 then (len - pos) else pos
    in l & listSelectedL .~ if len > 0
                            then Just newSel
                            else Nothing

-- | Return a list's selected element, if any.
listSelectedElement :: List e -> Maybe (Int, e)
listSelectedElement l = do
  sel <- l^.listSelectedL
  return (sel, (l^.listElementsL) !! sel)

-- Assuming `xs` is an existing list that we want to update to match the state
-- of `ys`. Given a selected index in `xs`, the goal is to compute the
-- corresponding index in `ys`.
maintainSel :: Eq e => [e] -> [e] -> Int -> Int
maintainSel xs ys sel = let hunks = getDiff xs ys
                        in merge 0 sel hunks

merge :: Eq e => Int -> Int -> [Diff e] -> Int
merge _   sel []                 = sel
merge idx sel (h:hs) | idx > sel = sel
                     | otherwise = case h of
    Both _ _ -> merge sel (idx+1) hs

    -- element removed in new list
    First _  -> let newSel = if idx < sel
                             then sel - 1
                             else sel
                in merge newSel idx hs

    -- element added in new list
    Second _ -> let newSel = if idx <= sel
                             then sel + 1
                             else sel
                in merge newSel (idx+1) hs
