{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.MenuBar
  ( MenuBar
  , SimpleMenuBar
  , DispatchingMenuBar

  , newMenuBar
  , hasOpenMenu
  , closeAllMenus
  , isMenuTitleEvent
  , handleMenuBarEvent

  , renderMenuBar
  )
where

import Control.Monad (when)
import Data.Maybe (isJust, listToMaybe, fromMaybe)
import Lens.Micro.Platform ((^.), (&), (%~), Lens', ix, each)
import Lens.Micro.Mtl

import qualified Data.Foldable as F
import qualified Data.Vector as V

import qualified Graphics.Vty as Vty

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Menu

-- | A menu bar holding a sequence of menus.
data MenuBar s n k =
    MenuBar { menuBarMenus :: !(V.Vector (Menu s n k))
            }

suffixLenses ''MenuBar

-- | A specialization of 'MenuBar' for menus with 'EventM' handlers; use
-- this with 'simpleMenu'.
type SimpleMenuBar s n = MenuBar s n (EventM n s ())

-- | A specialization of 'MenuBar' for menus with abstract key event
-- triggers; this with 'menuWithDispatcher'.
type DispatchingMenuBar s n k = MenuBar s n (EventM n s (EntryTrigger s n k))

-- | Create a new menu bar from the specified menu list. If the list is
-- empty, this calls 'error'.
newMenuBar :: [Menu s n k] -> MenuBar s n k
newMenuBar [] = error "BUG: newMenuBar requires a non-empty list"
newMenuBar ms = MenuBar $ V.fromList ms

-- | Return whether this menu bar has an open menu.
hasOpenMenu :: MenuBar s n k -> Bool
hasOpenMenu = isJust . getOpenMenu

-- | Get this menu bar's current open menu and its index, if any.
getOpenMenu :: MenuBar s n k -> Maybe (Int, Menu s n k)
getOpenMenu mb = do
    let ms = menuBarMenus mb
    idx <- V.findIndex menuIsOpen ms
    return (idx, ms V.! idx)

-- | Render this menu bar with the given application state as input.
renderMenuBar :: (Ord n) => s -> MenuBar s n k -> Widget n
renderMenuBar s mb =
    hBox $
    padLeft (Pad 1) <$>
    F.toList (renderMenu s <$> menuBarMenus mb)

-- | Is this event a title bar click event?
isMenuTitleEvent :: (Eq n) => MenuBar s n k -> BrickEvent n e -> Bool
isMenuTitleEvent mb (MouseDown n _ _ _) = isJust $ getMenuTitleMatch mb n
isMenuTitleEvent _ _ = False

-- | Given a resource name, find the menu whose title bar portion
-- matches the resource name, if any.
getMenuTitleMatch :: (Eq n) => MenuBar s n k -> n -> Maybe (Int, Menu s n k)
getMenuTitleMatch mb n =
    listToMaybe $ filter matchesTitle $ zip [0..] (F.toList $ mb^.menuBarMenusL)
    where
        matchesTitle (_, m) = n == menuTitleName m

-- | Handle an event for this menu bar and return @True@, or return
-- @False@ if the event was not handled (e.g. because the event was not
-- a menu title mouse click or because no menu was open to receive the
-- event).
handleMenuBarEvent :: (Eq n) => Lens' s (MenuBar s n k) -> BrickEvent n e -> EventM n s Bool
handleMenuBarEvent which e@(VtyEvent (Vty.EvKey Vty.KLeft [])) = do
    -- Since this key might be handled by the open menu, try that first
    -- and only switch menus if it wasn't handled by the menu.
    handled <- withOpenMenu which $ \(idx, _) ->
        handleMenuEvent (which.menuBarMenusL.ix idx) e

    when (not handled) $
        which %= openPreviousMenu

    return True
handleMenuBarEvent which e@(VtyEvent (Vty.EvKey Vty.KRight [])) = do
    -- Since this key might be handled by the open menu, try that first
    -- and only switch menus if it wasn't handled by the menu.
    handled <- withOpenMenu which $ \(idx, _) ->
        handleMenuEvent (which.menuBarMenusL.ix idx) e

    when (not handled) $
        which %= openNextMenu

    return True
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
                        which %= closeAllMenus
                        which %= openMenuIndex i
            return True
handleMenuBarEvent which e =
    withOpenMenu which $ \(idx, _) ->
        handleMenuEvent (which.menuBarMenusL.ix idx) e

-- | Given a menu bar with an open menu, switch the open menu to the one
-- preceding the currently open one, or do nothing if no menu is open.
openPreviousMenu :: MenuBar s n k -> MenuBar s n k
openPreviousMenu mb = fromMaybe mb $ do
    (i, _) <- getOpenMenu mb
    let newIndex = if i == 0
                   then V.length (mb^.menuBarMenusL) - 1
                   else i - 1
    return $ openMenuIndex newIndex $ closeAllMenus mb

-- | Given a menu bar with an open menu, switch the open menu to the one
-- following the currently open one, or do nothing if no menu is open.
openNextMenu :: MenuBar s n k -> MenuBar s n k
openNextMenu mb = fromMaybe mb $ do
    (i, _) <- getOpenMenu mb
    let newIndex = if i == V.length (mb^.menuBarMenusL) - 1
                   then 0
                   else i + 1
    return $ openMenuIndex newIndex mb

-- | Close all open menus in this menu bar.
closeAllMenus :: MenuBar s n k -> MenuBar s n k
closeAllMenus mb = mb & menuBarMenusL.each %~ closeMenu

-- | Open the menu in this menu bar with the specified index, if any.
openMenuIndex :: Int -> MenuBar s n k -> MenuBar s n k
openMenuIndex i mb = (closeAllMenus mb) & menuBarMenusL.ix i %~ openMenu

-- | Given a lens to access a menu bar and a handler to invoke on its
-- currently open menu, invoke the handler if there is an open menu and
-- return its result, or do nothing and return False otherwise.
withOpenMenu :: Lens' s (MenuBar s n k) -> ((Int, Menu s n k) -> EventM n s Bool) -> EventM n s Bool
withOpenMenu which f = do
    mb <- use which
    case getOpenMenu mb of
        Nothing -> return False
        Just pair -> f pair
