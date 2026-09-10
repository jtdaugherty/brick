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

import Control.Monad (when)
import Data.Maybe (isJust, listToMaybe)
import Lens.Micro.Platform ((^.), Lens', ix, each)
import Lens.Micro.Mtl

import qualified Data.Foldable as F
import qualified Data.Vector as V

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Menu

data MenuBar s n k =
    MenuBar { menuBarMenus :: V.Vector (Menu s n k)
            }

suffixLenses ''MenuBar

type SimpleMenuBar s n = MenuBar s n (EventM n s ())

newMenuBar :: [Menu s n k] -> MenuBar s n k
newMenuBar = MenuBar . V.fromList

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

handleMenuBarEvent :: (Eq n) => Lens' s (MenuBar s n k) -> BrickEvent n e -> EventM n s ()
handleMenuBarEvent which e@(MouseDown n _ _ _) = do
    mb <- use which
    case getMenuTitleMatch mb n of
        Nothing -> withOpenMenu which $ \(idx, _) ->
            handleMenuEvent (which.menuBarMenusL.ix idx) e
        Just (i, _) -> do
            mMatchingMenu <- preuse (which.menuBarMenusL.ix i)
            case mMatchingMenu of
                Nothing -> return ()
                Just matchingMenu ->
                    when (not $ menuIsOpen matchingMenu) $ do
                        closeAllMenus which
                        which.menuBarMenusL.ix i %= openMenu
handleMenuBarEvent which e =
    withOpenMenu which $ \(idx, _) ->
        handleMenuEvent (which.menuBarMenusL.ix idx) e

closeAllMenus :: Lens' s (MenuBar s n k) -> EventM n s ()
closeAllMenus which =
    which.menuBarMenusL.each %= closeMenu

withOpenMenu :: Lens' s (MenuBar s n k) -> ((Int, Menu s n k) -> EventM n s ()) -> EventM n s ()
withOpenMenu which f = do
    mb <- use which
    case getOpenMenu mb of
        Nothing -> return ()
        Just pair -> f pair
