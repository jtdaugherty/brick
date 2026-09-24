{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- | This module provides a menu bar for grouping menus together.
--
-- Menu bars carry menus of a particular type using the menu types
-- provided in the @Brick.Widgets.Menu@ module. The type aliases
-- provided here correspond to the aliases for menu use cases:
--
-- * 'SimpleMenuBar': a menu bar made up of 'SimpleMenu's created with
--   'simpleMenu'
-- * 'DispatchingMenuBar': a menu bar made up of 'DispatchingMenu's
--    created with 'menuWithDispatcher'
-- * 'MenuBar': the fully general type for menu bars with menus created
--   with 'menu'
--
-- In all cases, use 'newMenuBar' to construct a menu bar, and create
-- its menus using the corresponding menu constructor for the type of
-- menu bar you want to make.
--
-- Render your menu bar with 'renderMenuBar' and handle menu bar events
-- with 'handleMenuBarEvent', deferring to your application's event
-- handling for events that the menu bar doesn't handle.
--
-- Similar to individual menus, menu bars have an orientation that can
-- be changed with 'setMenuBarOrientation'.
--
-- This API requires the use of lenses for application state fields that
-- store menu bar state.
--
-- See the @MenuBarDemo@ demonstration program for a complete working
-- example of using this API.
module Brick.Widgets.MenuBar
  (
  -- * Types
    MenuBar
  , SimpleMenuBar
  , DispatchingMenuBar

  -- * Creating menu bars
  , newMenuBar

  -- * Handling events
  , handleMenuBarEvent

  -- * Rendering
  , renderMenuBar

  -- * Working with menu bars
  , hasOpenMenu
  , closeAllMenus
  , openMenuAtIndex
  , toggleMenuAtIndex
  , setMenuBarOrientation
  )
where

import Control.Monad (when)
import Data.Maybe (isJust, listToMaybe, fromMaybe)
import Lens.Micro.Platform ((^.), (&), (%~), (.~), Lens', ix, each)
import Lens.Micro.Mtl

import qualified Data.Foldable as F
import qualified Data.Vector as V

import qualified Graphics.Vty as Vty

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Menu

-- | A menu bar holding a sequence of menus.
--
-- A menu bar can have up to one open menu at a time.
data MenuBar s n k =
    MenuBar { menuBarOrientation :: !MenuOrientation
            , menuBarMenus :: !(V.Vector (Menu s n k))
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
newMenuBar ms = MenuBar LeftToRight $ V.fromList ms

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
    withDefAttr menuTitleAttr $ padForOrientation body
    where
        padForOrientation = case mb^.menuBarOrientationL of
            LeftToRight -> padRight Max
            RightToLeft -> padLeft Max . padRight (Pad 1)

        body = hBox $
               padLeft (Pad 1) <$>
               F.toList (renderMenu s <$> menuBarMenus mb)

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
--
-- Events handled include:
--
-- * Mouse clicks on menu titles will open the clicked menu, closing
--   other open menus.
-- * Left and Right arrow keys will cycle between menus if there is an
--   open menu.
-- * If a submenu entry is selected, the arrow keys will open it or
--   close it if it is open, depending on the configured menu bar
--   orientation.
-- * @Esc@ will close the currently-open menu.
--
-- In all other cases, this will attempt to defer to the opened menu to
-- handle the event. This returns @True@ if the event was one of the
-- above and was handled, @True@ if the event was not one of the above
-- but was handled by the open menu, or @False@ otherwise.
--
-- A return value of @True@ indicates that the event should not be
-- handled by the application because it was destined for the menu bar
-- or one of its menus; a return value of @False@ indicates that the
-- event should be handled by the application because it did not affect
-- the menu bar or its menus in their current state for any reason.
-- Consequently, a common pattern when using this function will look
-- something like this:
--
-- @
-- myApplicationEventHandler :: BrickEvent n e -> EventM n s ()
-- myApplicationEventHandler e = do
--     handled <- handleMenuBarEvent myMenuBarLens e
--     when (not handled) $ do
--         -- Go on to handle the event in the rest of the application
-- @
handleMenuBarEvent :: (Eq n)
                   => Lens' s (MenuBar s n k)
                   -- ^ The lens into the application state where the
                   -- menu state can be found
                   -> BrickEvent n e
                   -- ^ The event to handle
                   -> EventM n s Bool
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
                        which %= openMenuAtIndex i
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
    return $ openMenuAtIndex newIndex $ closeAllMenus mb

-- | Given a menu bar with an open menu, switch the open menu to the one
-- following the currently open one, or do nothing if no menu is open.
openNextMenu :: MenuBar s n k -> MenuBar s n k
openNextMenu mb = fromMaybe mb $ do
    (i, _) <- getOpenMenu mb
    let newIndex = if i == V.length (mb^.menuBarMenusL) - 1
                   then 0
                   else i + 1
    return $ openMenuAtIndex newIndex mb

-- | Close all open menus in this menu bar.
closeAllMenus :: MenuBar s n k -> MenuBar s n k
closeAllMenus mb = mb & menuBarMenusL.each %~ closeMenu

-- | Open the menu at the specified index, closing any other open menus
-- in the menu bar. If the index is invalid, this does nothing.
openMenuAtIndex :: Int -> MenuBar s n k -> MenuBar s n k
openMenuAtIndex i mb = (closeAllMenus mb) & menuBarMenusL.ix i %~ openMenu

-- | Set the menu bar's orientation, including all of its menus. For
-- details, see 'setMenuOrientation'.
setMenuBarOrientation :: MenuOrientation -> MenuBar s n k -> MenuBar s n k
setMenuBarOrientation o mb = mb & menuBarOrientationL .~ o
                                & menuBarMenusL.each %~ setMenuOrientation o

-- | Toggle the open state of the menu at the specified index. If
-- toggling to open, this will close any other open menus in the menu
-- bar. If the index is invalid, this does nothing.
toggleMenuAtIndex :: Int -> MenuBar s n k -> MenuBar s n k
toggleMenuAtIndex i mb =
    case getOpenMenu mb of
        Nothing -> openMenuAtIndex i mb
        Just (idx, _) -> if idx == i
                         then closeAllMenus mb
                         else openMenuAtIndex i mb

-- | Given a lens to access a menu bar and a handler to invoke on its
-- currently open menu, invoke the handler if there is an open menu and
-- return its result, or do nothing and return False otherwise.
withOpenMenu :: Lens' s (MenuBar s n k) -> ((Int, Menu s n k) -> EventM n s Bool) -> EventM n s Bool
withOpenMenu which f = do
    mb <- use which
    case getOpenMenu mb of
        Nothing -> return False
        Just pair -> f pair
