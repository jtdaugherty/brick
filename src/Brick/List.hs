{-# LANGUAGE RankNTypes #-}
module Brick.List
  ( List(listElements)
  , list
  , moveBy
  , drawList
  , listInsert
  )
where

import Control.Applicative ((<$>), (<|>))
import Control.Lens (Lens')
import Data.Default
import Data.Maybe (catMaybes)
import Graphics.Vty (Event(..), Key(..), DisplayRegion)
import qualified Data.Map as M

import Brick.Core (HandleEvent(..), SetSize(..))
import Brick.Prim (Prim(..), Priority(..), (<<=), (<<+))
import Brick.Scroll (VScroll, vScroll, scrollToView)
import Brick.Util (clamp, for)

data List e =
    List { listElements :: ![e]
         , listElementDraw :: Bool -> e -> Prim (List e)
         , listSelected :: !(Maybe Int)
         , listScroll :: !VScroll
         , listElementHeights :: M.Map Int Int
         }

instance HandleEvent (List e) where
    handleEvent e theList = f theList
        where
            f = case e of
                  EvKey KUp [] -> moveUp
                  EvKey KDown [] -> moveDown
                  _ -> id

instance SetSize (List e) where
    setSize sz l =
        let updatedScroll = setSize sz $ listScroll l
        in ensureSelectedVisible $ l { listScroll = updatedScroll }

list :: (Bool -> e -> Prim (List e)) -> [e] -> List e
list draw es =
    let selIndex = if null es then Nothing else Just 0
    in List es draw selIndex def M.empty

listSetElementSize :: Int -> DisplayRegion -> List e -> List e
listSetElementSize i sz l =
    l { listElementHeights = M.insert i (snd sz) (listElementHeights l)
      }

drawList :: Lens' a (List e) -> Prim a
drawList lens =
    With lens $ \l ->
      let es = listElements l
          drawn = for (zip [0..] es) $ \(i, e) ->
                    let isSelected = Just i == listSelected l
                        elemPrim = listElementDraw l isSelected e
                    in ( SaveSize (listSetElementSize i) elemPrim
                       , High
                       )
      in SaveSize setSize $
         vScroll (listScroll l) $
         (VBox drawn <<= VPad ' ') <<+ HPad ' '

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
    in ensureSelectedVisible $ l { listSelected = Just newSel
                                 , listElements = front ++ (e : back)
                                 }

moveUp :: List e -> List e
moveUp = moveBy (-1)

moveDown :: List e -> List e
moveDown = moveBy 1

moveBy :: Int -> List e -> List e
moveBy amt l =
    let newSel = clamp 0 (length (listElements l) - 1) <$> (amt +) <$> listSelected l
    in ensureSelectedVisible $ l { listSelected = newSel }

ensureSelectedVisible :: List e -> List e
ensureSelectedVisible l =
    let Just scrollTo = (listSelected l) <|> (Just 0)
        heights = listElementHeights l
        scrollTop = sum $ catMaybes $ (\k -> M.lookup k heights) <$> [0..scrollTo-1]
        scrollBottom = case M.lookup scrollTo heights of
            Nothing -> 1
            Just k -> k
    in l { listScroll = scrollToView (scrollTop, scrollBottom) (listScroll l)
         }
