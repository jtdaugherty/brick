{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.MenuBar
  ( MenuBar
  , SimpleMenuBar

  , newMenuBar
  , hasOpenMenu
  , isMenuTitleEvent
  , handleMenuBarEvent

  , renderMenuBar
  )
where

import Control.Monad (when, void)
import Data.Maybe (isJust, listToMaybe, fromMaybe)
import Lens.Micro.Platform ((^.), (&), (%~), Lens', ix, each)
import Lens.Micro.Mtl

import qualified Data.Foldable as F
import qualified Data.Vector as V

import qualified Graphics.Vty as Vty

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Menu

data MenuBar s n k =
    MenuBar { menuBarMenus :: V.Vector (Menu s n k)
            }

suffixLenses ''MenuBar

type SimpleMenuBar s n = MenuBar s n (EventM n s ())

newMenuBar :: [Menu s n k] -> MenuBar s n k
newMenuBar [] = error "BUG: newMenuBar requires a non-empty list"
newMenuBar ms = MenuBar $ V.fromList ms

hasOpenMenu :: MenuBar s n k -> Bool
hasOpenMenu = isJust . getOpenMenu

getOpenMenu :: MenuBar s n k -> Maybe (Int, Menu s n k)
getOpenMenu mb = do
    let ms = menuBarMenus mb
    idx <- V.findIndex menuIsOpen ms
    return (idx, ms V.! idx)

renderMenuBar :: (Ord n) => s -> MenuBar s n k -> Widget n
renderMenuBar s mb =
    hBox $
    padLeft (Pad 1) <$>
    F.toList (renderMenu s <$> menuBarMenus mb)

isMenuTitleEvent :: (Eq n) => MenuBar s n k -> BrickEvent n e -> Bool
isMenuTitleEvent mb (MouseDown n _ _ _) = isJust $ getMenuTitleMatch mb n
isMenuTitleEvent _ _ = False

getMenuTitleMatch :: (Eq n) => MenuBar s n k -> n -> Maybe (Int, Menu s n k)
getMenuTitleMatch mb n =
    listToMaybe $ filter matchesTitle $ zip [0..] (F.toList $ mb^.menuBarMenusL)
    where
        matchesTitle (_, m) = n == menuTitleName m

handleMenuBarEvent :: (Eq n) => Lens' s (MenuBar s n k) -> BrickEvent n e -> EventM n s Bool
handleMenuBarEvent which (VtyEvent (Vty.EvKey Vty.KLeft [])) = do
    which %= openPreviousMenu
    return True
handleMenuBarEvent which (VtyEvent (Vty.EvKey Vty.KRight [])) = do
    which %= openNextMenu
    return True
handleMenuBarEvent which e@(MouseDown n _ _ _) = do
    mb <- use which
    case getMenuTitleMatch mb n of
        Nothing -> withOpenMenu which $ \(idx, _) ->
            void $ handleMenuEvent (which.menuBarMenusL.ix idx) e
        Just (i, _) -> do
            mMatchingMenu <- preuse (which.menuBarMenusL.ix i)
            case mMatchingMenu of
                Nothing -> return ()
                Just matchingMenu ->
                    when (not $ menuIsOpen matchingMenu) $ do
                        which %= closeAllMenus
                        which %= openMenuIndex i
            return True
handleMenuBarEvent which e =
    withOpenMenu which $ \(idx, _) ->
        void $ handleMenuEvent (which.menuBarMenusL.ix idx) e

openPreviousMenu :: MenuBar s n k -> MenuBar s n k
openPreviousMenu mb = fromMaybe mb $ do
    (i, _) <- getOpenMenu mb
    let newIndex = if i == 0
                   then V.length (mb^.menuBarMenusL) - 1
                   else i - 1
    return $ openMenuIndex newIndex $ closeAllMenus mb

openNextMenu :: MenuBar s n k -> MenuBar s n k
openNextMenu mb = fromMaybe mb $ do
    (i, _) <- getOpenMenu mb
    let newIndex = if i == V.length (mb^.menuBarMenusL) - 1
                   then 0
                   else i + 1
    return $ openMenuIndex newIndex mb

closeAllMenus :: MenuBar s n k -> MenuBar s n k
closeAllMenus mb = mb & menuBarMenusL.each %~ closeMenu

openMenuIndex :: Int -> MenuBar s n k -> MenuBar s n k
openMenuIndex i mb = (closeAllMenus mb) & menuBarMenusL.ix i %~ openMenu

withOpenMenu :: Lens' s (MenuBar s n k) -> ((Int, Menu s n k) -> EventM n s ()) -> EventM n s Bool
withOpenMenu which f = do
    mb <- use which
    case getOpenMenu mb of
        Nothing -> return False
        Just pair -> f pair >> return True
