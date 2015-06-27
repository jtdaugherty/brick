{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.List
  ( List(listElements)
  , list
  , renderList
  , listMoveBy
  , listMoveTo
  , listMoveUp
  , listMoveDown
  , listInsert
  , listRemove
  , listReplace
  , listSelectedElement

  , listAttr
  , listSelectedAttr
  )
where

import Control.Applicative ((<$>))
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Graphics.Vty (Event(..), Key(..))

import Brick.Core (HandleEvent(..), Name)
import Brick.Merge (maintainSel)
import Brick.Widgets.Core
import Brick.Util (clamp, for)
import Brick.AttrMap

data List e =
    List { listElements :: ![e]
         , listElementDraw :: Bool -> e -> Widget
         , listSelected :: !(Maybe Int)
         , listName :: Name
         }

instance HandleEvent (List e) where
    handleEvent e theList = f theList
        where
            f = case e of
                  EvKey KUp [] -> listMoveUp
                  EvKey KDown [] -> listMoveDown
                  _ -> id

listAttr :: AttrName
listAttr = "list"

listSelectedAttr :: AttrName
listSelectedAttr = listAttr <> "selected"

list :: Name -> (Bool -> e -> Widget) -> [e] -> List e
list name draw es =
    let selIndex = if null es then Nothing else Just 0
    in List es draw selIndex name

renderList :: List e -> Widget
renderList l = withAttrName listAttr $
               viewport (listName l) Vertical $
               vBox $
               drawListElements l

drawListElements :: List e -> [Widget]
drawListElements l = drawnElements
    where
        es = listElements l
        drawnElements = for (zip [0..] es) $ \(i, e) ->
            let isSelected = Just i == listSelected l
                elemWidget = let w = listElementDraw l isSelected e
                             in if vSize w == Unlimited
                                then error "Tried to add a list widget that has has an unlimited vertical size"
                                else w
                makeVisible = if isSelected
                              then (visible . withAttrName listSelectedAttr)
                              else id
            in makeVisible elemWidget

listInsert :: Int -> e -> List e -> List e
listInsert pos e l =
    let safePos = clamp 0 (length es) pos
        es = listElements l
        newSel = case listSelected l of
          Nothing -> 0
          Just s -> if safePos < s
                    then s + 1
                    else s
        (front, back) = splitAt safePos es
    in l { listSelected = Just newSel
         , listElements = front ++ (e : back)
         }

listRemove :: Int -> List e -> List e
listRemove pos l | null es = l
                 | pos /= clamp 0 (length es - 1) pos = l
                 | otherwise =
    let newSel = case listSelected l of
          Nothing -> 0
          Just s  -> if pos < s
                     then s - 1
                     else s
        (front, back) = splitAt pos es
        es' = front ++ tail back
    in l { listSelected = if null es'
                          then Nothing
                          else Just newSel
         , listElements = es'
         }
    where
        es = listElements l

-- Replaces entire list with a new set of elements, but preserves selected index
-- using a two-way merge algorithm.
listReplace :: Eq e => [e] -> List e -> List e
listReplace es' l | es' == es = l
                  | otherwise =
    let sel = fromMaybe 0 (listSelected l)
        newSel = case (null es, null es') of
          (_, True)      -> Nothing
          (True, False)  -> Just 0
          (False, False) -> Just (maintainSel es es' sel)
    in l { listSelected = newSel
         , listElements = es'
         }
    where
        es = listElements l

listMoveUp :: List e -> List e
listMoveUp = listMoveBy (-1)

listMoveDown :: List e -> List e
listMoveDown = listMoveBy 1

listMoveBy :: Int -> List e -> List e
listMoveBy amt l =
    let newSel = clamp 0 (length (listElements l) - 1) <$> (amt +) <$> listSelected l
    in l { listSelected = newSel }

listMoveTo :: Int -> List e -> List e
listMoveTo pos l =
    let len = length (listElements l)
        newSel = clamp 0 (len - 1) $ if pos < 0 then (len - pos) else pos
    in l { listSelected = if len > 0
                          then Just newSel
                          else Nothing
         }

listSelectedElement :: List e -> Maybe (Int, e)
listSelectedElement l = do
  sel <- listSelected l
  return (sel, listElements l !! sel)
