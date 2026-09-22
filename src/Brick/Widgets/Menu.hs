{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
module Brick.Widgets.Menu
  ( Menu
  , menuIsOpen
  , menuContentWidth
  , menuTitleName
  , MenuRegion(..)
  , openMenu
  , closeMenu
  , toggleMenu

  -- * Constructing menus and items
  , menu
  , MenuItem
  , MenuEntry
  , menuEntry
  , menuSeparator
  , menuGap

  -- * Configuring menus
  , setDefaultEntryRenderer
  , setTitleRenderer

  -- * Configuring menu items
  , setEnabledWith
  , setEntryRenderer

  -- * Menus with EventM handlers
  , SimpleMenu
  , SimpleMenuItem
  , simpleMenu

  -- * Menus with custom keybindings
  , DispatchingMenu
  , DispatchingMenuItem
  , EntryTrigger
  , menuWithDispatcher
  , menuEntryForKey
  , menuEntryForEvent
  , menuEntryForAction

  -- * Handling events
  , handleMenuEvent

  -- * Rendering menus
  , renderMenu

  -- * Attributes
  , menuAttr
  , menuTitleAttr
  , menuTitleSelectedAttr
  , menuBodyAttr
  , menuEntryDisabledAttr
  , menuEntrySelectedAttr
  , menuEntrySelectedDisabledAttr
  , menuEntryKeybindingAttr
  )
where

import Control.Monad (when)

import Lens.Micro ((^.), (.~), (&), Traversal')
import Lens.Micro.Mtl

import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Maybe (listToMaybe, fromMaybe)

import qualified Graphics.Vty as Vty

import Brick.AttrMap
import Brick.Types
import Brick.Widgets.Border
import Brick.Widgets.Core

import Brick.Keybindings.KeyDispatcher
import Brick.Keybindings.KeyConfig
import Brick.Keybindings.Pretty

-- | The type of menu regions for embedding in the application's
-- resource name and reporting mouse click events.
data MenuRegion =
    MenuTitle
    -- ^ The region of a menu's title
    | MenuBody
    -- ^ The region of a menu's body
    deriving (Ord, Show, Eq)

-- | A menu.
--
-- Menus are parameterized on three types:
--
-- * @s@: the application state type used in the @App@ type,
-- * @n@: the application resource name type used in the application's
--   @Widget@ type, and
-- * @k@: the type of data carried and handled by the menu's event
--   handler when a menu entry has been activated.
--
-- Menus contain a sequence of /items/ of type 'MenuItem'. See the
-- constructors below for both menus and menu items to create menus.
data Menu s n k =
    Menu { menuTitle :: !T.Text
         -- ^ The menu's title
         , menuTitleRenderer :: T.Text -> Widget n
         -- ^ The renderer for the menu's title
         , menuItems :: !(V.Vector (MenuItem s n k))
         -- ^ The contents of the menu
         , menuIsOpen :: !Bool
         -- ^ Whether the menu is open
         , menuContentWidth :: !Int
         -- ^ The width of the menu's items within the enclosing border.
         -- This is a record accessor so it can also be used to change
         -- the menu's width.
         , menuTitleName :: !n
         -- ^ The resource name for this menu's title for generating and
         -- detecting mouse click events
         , menuRegionNameBuilder :: MenuRegion -> n
         -- ^ A function to build resource names for clickable regions
         , menuSelectedIndex :: !(Maybe Int)
         -- ^ State for tracking the selected item index, if any
         , menuEventHandler :: k -> EventM n s ()
         -- ^ Handler to be invoked when an entry in this menu is
         -- activated
         , menuFallbackEventHandler :: Vty.Key -> [Vty.Modifier] -> EventM n s Bool
         -- ^ Handler for key events that weren't handled by
         -- 'handleMenuEvent'
         , menuEntryDefaultRenderer :: k -> T.Text -> Widget n
         -- ^ The function to render entries in this menu
         }

-- | The type of menu items.
data MenuItem s n k =
    MISeparator
    -- ^ A horizontal border between menu items
    | MIGap
    -- ^ An empty line between menu items
    | MIEntry !(MenuEntry s n k)
    -- ^ A labeled menu entry that can be activated with the mouse or by
    -- a keypress

-- | A labeled menu entry that can be activated with the mouse or by a
-- keypress.
data MenuEntry s n k =
    MenuEntry { menuEntryLabel :: !T.Text
              -- ^ The menu entry's label
              , menuEntryEnabled :: s -> Bool
              -- ^ The function to determine whether this menu entry is
              -- enabled
              , menuEntryEvent :: !k
              -- ^ The event to generate when this entry is activated
              , menuEntryRenderer :: Maybe (k -> T.Text -> Widget n)
              -- ^ This menu entry's renderer
              }

suffixLenses ''Menu

-- | Set this menu entry's function used to check for its enabled state.
setEnabledWith :: (s -> Bool) -> MenuItem s n k -> MenuItem s n k
setEnabledWith f = mapMenuEntry (\e -> e { menuEntryEnabled = f })

-- | Set this menu entry's rendering function, overriding the menu's
-- default rendering behavior for this entry.
setEntryRenderer :: (k -> T.Text -> Widget n) -> MenuItem s n k -> MenuItem s n k
setEntryRenderer f = mapMenuEntry (\e -> e { menuEntryRenderer = Just f })

-- | Set this menu's entry rendering function.
setDefaultEntryRenderer :: (k -> T.Text -> Widget n) -> Menu s n k -> Menu s n k
setDefaultEntryRenderer f m = m { menuEntryDefaultRenderer = f }

-- | Set this menu's title renderer.
setTitleRenderer :: (T.Text -> Widget n) -> Menu s n k -> Menu s n k
setTitleRenderer f m = m { menuTitleRenderer = f }

mapMenuEntry :: (MenuEntry s n k -> MenuEntry s n k) -> MenuItem s n k -> MenuItem s n k
mapMenuEntry f (MIEntry e) = MIEntry $ f e
mapMenuEntry _ e = e

-- | A separator between menu items.
menuSeparator :: MenuItem s n k
menuSeparator = MISeparator

-- | A gap between menu items.
menuGap :: MenuItem s n k
menuGap = MIGap

-- | Create a menu entry with the specified label and event data.
-- When the entry is activated, its event data will be passed to the
-- event handler of the enclosing menu.
--
-- By default, this entry has no custom renderer so its appearance is
-- determined by the default entry renderer of the enclosing menu.
-- To change either of these behaviors, use 'setEntryRenderer' or
-- 'setDefaultEntryRenderer'.
--
-- By default, this entry is always enabled regardless of the
-- application state. To change this, use 'setEnabledWith'.
--
-- This is the fully general entry constructor. For more specific use
-- cases, see the other 'MenuItem' constructors in this module.
menuEntry :: T.Text
          -- ^ The menu entry's label
          -> k
          -- ^ The event data carried by the menu entry that will be
          -- passed to the enclosing menu's event handler when this
          -- entry is activated
          -> MenuItem s n k
menuEntry label ev =
    MIEntry $ MenuEntry { menuEntryLabel = label
                        , menuEntryEnabled = const True
                        , menuEntryEvent = ev
                        , menuEntryRenderer = Nothing
                        }

-- | A specialization of 'Menu' that has 'EventM' handlers in each menu
-- entry that are evaluated whenever the entries are activated.
type SimpleMenu s n = Menu s n (EventM n s ())

-- | A specialization of 'MenuItem' for 'SimpleMenu' for entries with
-- 'EventM' handlers that are evaluated whenever the entries are
-- activated.
type SimpleMenuItem s n = MenuItem s n (EventM n s ())

-- | Create a 'SimpleMenu'.
simpleMenu :: T.Text
           -- ^ The menu's title
           -> (MenuRegion -> n)
           -- ^ The menu's resource name constructor
           -> [SimpleMenuItem s n]
           -- ^ The items in this menu
           -> SimpleMenu s n
simpleMenu title regionNameBuilder items =
    menu title regionNameBuilder items id

defaultMenuPadding :: Int
defaultMenuPadding = 7

-- | Create a 'Menu'.
menu :: T.Text
     -- ^ The menu's title
     -> (MenuRegion -> n)
     -- ^ The menu's resource name constructor
     -> [MenuItem s n k]
     -- ^ The items in this menu
     -> (k -> EventM n s ())
     -- ^ The event handler to invoke when entries are activated
     -> Menu s n k
menu title regionNameBuilder items handler =
    let defaultWidth = (maximum $ menuItemWidth <$> items) + defaultMenuPadding
    in Menu { menuTitle = title
            , menuTitleRenderer = txt
            , menuItems = V.fromList items
            , menuIsOpen = False
            , menuContentWidth = defaultWidth
            , menuTitleName = regionNameBuilder MenuTitle
            , menuRegionNameBuilder = regionNameBuilder
            , menuSelectedIndex = Nothing
            , menuEventHandler = handler
            , menuFallbackEventHandler = const $ const $ return False
            , menuEntryDefaultRenderer = \_ label -> txt label
            }

-- | A trigger to be executed when an entry with this trigger is
-- activated.
data EntryTrigger s n k =
    TriggerEvent !(EventTrigger k)
    -- ^ The entry triggers an abstract 'EventTrigger'
    | TriggerAction !(EventM n s ())
    -- ^ The entry triggers a specific 'EventM' action

-- | A specialization of 'Menu' whose entries are associated with
-- specific keys or abstract key events handled by a 'KeyDispatcher'.
type DispatchingMenu s n k = Menu s n (EntryTrigger s n k)

-- | A specialization of 'MenuItem' for menus whose entries are
-- associated with specific keys or abstract key events handled by a
-- 'KeyDispatcher'.
type DispatchingMenuItem s n k = MenuItem s n (EntryTrigger s n k)

-- | Create a 'Menu' whose entries are activated by specific triggers,
-- including specified key bindings or abstract key events associated
-- with a 'KeyDispatcher'.
--
-- To create entries in this menu, see 'menuEntryForKey',
-- 'menuEntryForEvent', and 'menuEntryForAction'.
menuWithDispatcher :: (Eq k)
                   => KeyDispatcher k (EventM n s)
                   -- ^ The key dispatcher to use to build the menu, and
                   -- whose handlers should be invoked by the menu's
                   -- entries
                   -> T.Text
                   -- ^ The menu's title
                   -> (MenuRegion -> n)
                   -- ^ The menu's resource name constructor
                   -> [DispatchingMenuItem s n k]
                   -- ^ The items in this menu
                   -> DispatchingMenu s n k
menuWithDispatcher kd title regionNameBuilder items =
    setWidth $
    addFallbackHandler $
    setDefaultEntryRenderer renderWithKeybinding $
    menu title regionNameBuilder items handler
    where
        setWidth m =
            m { menuContentWidth = menuContentWidth m + 4 }

        addFallbackHandler m =
            m { menuFallbackEventHandler = handleKey kd }

        renderWithKeybinding e label =
            let maybeShowKeybinding w = fromMaybe w $ do
                    keybinding <- case e of
                        TriggerEvent (ByKey b) -> return b
                        TriggerEvent (ByEvent ev) -> listToMaybe $ bindingsForEvent ev
                        TriggerAction {} -> Nothing

                    return $ w <+> (withDefAttr menuEntryKeybindingAttr $
                                    txt $ ppBinding keybinding)

            in maybeShowKeybinding $ padRight Max $ txt label

        bindingsForEvent ev =
            [ b | KeyHandler { khBinding = b, khHandler = h } <- snd <$> keyDispatcherToList kd, kehEventTrigger h == ByEvent ev ]

        handler trigger =
            case trigger of
                  TriggerEvent (ByKey b)    -> invokeHandler $ lookupVtyEvent (kbKey b) (F.toList $ kbMods b) kd
                  TriggerEvent (ByEvent ev) -> invokeHandler $ lookupEvent ev kd
                  TriggerAction act         -> act
            where
                invokeHandler Nothing = return ()
                invokeHandler (Just kh) = handlerAction $ kehHandler $ khHandler kh

-- | Create a menu entry that is activated by the specified key binding,
-- irrespective of the enclosing menu's 'KeyDispatcher' configuration.
menuEntryForKey :: T.Text
                -- ^ The menu entry's label
                -> Binding
                -- ^ The specific key binding to trigger this menu entry
                -> DispatchingMenuItem s n k
menuEntryForKey label b = menuEntry label $ TriggerEvent $ ByKey b

-- | Create a menu entry that generates the specified abstract key event
-- when activated, thus triggering the enclosing menu's 'KeyDispatcher'
-- handler for that event.
menuEntryForEvent :: T.Text
                  -- ^ The menu entry's label
                  -> k
                  -- ^ The abstract key event to generate when this
                  -- entry is activated
                  -> DispatchingMenuItem s n k
menuEntryForEvent label ev = menuEntry label $ TriggerEvent $ ByEvent ev

-- | Create a menu entry that invokes the specified 'EventM' action when
-- activated. Use this for entries that are not invoked by specific keys
-- or associated with abstract key events.
menuEntryForAction :: T.Text
                   -- ^ The menu entry's label
                   -> EventM n s ()
                   -- ^ The action to evaluate when this entry is
                   -- activated
                   -> DispatchingMenuItem s n k
menuEntryForAction label act = menuEntry label $ TriggerAction act

-- | Close a menu and unselect any selected entry.
closeMenu :: Menu s n k -> Menu s n k
closeMenu m = m & menuIsOpenL .~ False
                & menuSelectedIndexL .~ Nothing

-- | Open a menu.
openMenu :: Menu s n k -> Menu s n k
openMenu m = m & menuIsOpenL .~ True

-- | Toggle the menu's open state.
toggleMenu :: Menu s n k -> Menu s n k
toggleMenu m =
    if m^.menuIsOpenL
    then closeMenu m
    else openMenu m

-- | Get the screen width of this menu item if it is an entry; zero
-- otherwise.
menuItemWidth :: MenuItem s n k -> Int
menuItemWidth MISeparator = 0
menuItemWidth MIGap = 0
menuItemWidth (MIEntry e) = menuEntryWidth e

-- | Get this entry's width, i.e., the width of its label.
menuEntryWidth :: MenuEntry s n k -> Int
menuEntryWidth = textWidth . menuEntryLabel

-- | Render a menu.
renderMenu :: (Ord n) => s -> Menu s n k -> Widget n
renderMenu s m =
    if menuIsOpen m
    then (translateLayer (Location (-1, 1)) body) `above` title
    else title
    where
        setTitleAttr = if menuIsOpen m
                       then withDefAttr menuTitleSelectedAttr
                       else withDefAttr menuTitleAttr
        title = clickable (menuTitleName m) $
                setTitleAttr $
                menuTitleRenderer m $
                menuTitle m

        body = joinBorders $
               border $
               hLimit (menuContentWidth m) $
               clickable (menuRegionNameBuilder m MenuBody) $
               vBox $
               renderMenuItem <$> (zip [0..] $ V.toList $ menuItems m)

        renderMenuItem (_, MISeparator) = hBorder
        renderMenuItem (_, MIGap)       = vLimit 1 $ fill ' '
        renderMenuItem (i, MIEntry e)   = renderMenuEntry i e

        renderMenuEntry i e =
            let renderEntry = fromMaybe (menuEntryDefaultRenderer m) (menuEntryRenderer e)
            in setEntryAttr i e $
               vLimit 1 $
               padRight (Pad 1) $
               padRight Max $
               padLeft (Pad 1) $
               renderEntry (menuEntryEvent e) (menuEntryLabel e)

        setEntryAttr i e =
            if Just i == menuSelectedIndex m
            then if menuEntryEnabled e s
                 then forceAttr menuEntrySelectedAttr
                 else forceAttr menuEntrySelectedDisabledAttr
            else if menuEntryEnabled e s
                 then id
                 else forceAttr menuEntryDisabledAttr

-- | The base attribute of menus.
menuAttr :: AttrName
menuAttr = attrName "brick" <> attrName "menu"

-- | Menu titles.
menuTitleAttr :: AttrName
menuTitleAttr = menuAttr <> attrName "title"

-- | Selected menu titles, for open menus.
menuTitleSelectedAttr :: AttrName
menuTitleSelectedAttr = menuTitleAttr <> attrName "selected"

-- | The base attribute for menu bodies.
menuBodyAttr :: AttrName
menuBodyAttr = menuAttr <> attrName "body"

-- | Menu entry keybindings for entries in menus created with
-- 'menuWithDispatcher'.
menuEntryKeybindingAttr :: AttrName
menuEntryKeybindingAttr = menuBodyAttr <> attrName "keybinding"

-- | Disabled menu entries.
menuEntryDisabledAttr :: AttrName
menuEntryDisabledAttr = menuBodyAttr <> attrName "disabled"

-- | Selected and enabled menu entries.
menuEntrySelectedAttr :: AttrName
menuEntrySelectedAttr = menuBodyAttr <> attrName "selected"

-- | Selected and disnabled menu entries.
menuEntrySelectedDisabledAttr :: AttrName
menuEntrySelectedDisabledAttr = menuEntrySelectedAttr <> attrName "disabled"

-- | Select the next entry in a menu, or the first one if no entry is
-- currently selected.
selectNextEntry :: Menu s n k -> Menu s n k
selectNextEntry m =
    case matching V.!? 0 of
        Nothing -> m
        Just (newIdx, _) -> m & menuSelectedIndexL .~ Just newIdx
    where
        dropAmt = case m^.menuSelectedIndexL of
                 Nothing -> 0
                 Just i -> i + 1
        is = m^.menuItemsL
        matching = V.filter (itemIsSelectable . snd) items
        pairs = V.zip (V.enumFromN 0 (V.length is)) is
        items = V.drop dropAmt $ pairs <> pairs

itemIsSelectable :: MenuItem s n k -> Bool
itemIsSelectable (MIEntry {}) = True
itemIsSelectable _ = False

-- | Select the prevouis entry in a menu, or the last one if no entry is
-- currently selected.
selectPrevEntry :: Menu s n k -> Menu s n k
selectPrevEntry m =
    case matching V.!? 0 of
        Nothing -> m
        Just (newIdx, _) -> m & menuSelectedIndexL .~ Just newIdx
    where
        takeAmt = fromMaybe 0 $ m^.menuSelectedIndexL
        is = m^.menuItemsL
        matching = V.filter (itemIsSelectable . snd) items
        pairs = V.zip (V.enumFromN 0 (V.length is)) is
        items = V.reverse $ pairs <> V.take takeAmt pairs

withMenu :: Traversal' s (Menu s n k) -> (Menu s n k -> EventM n s Bool) -> EventM n s Bool
withMenu which f = do
    mMenu <- preuse which
    case mMenu of
        Nothing -> return False
        Just m -> f m

-- | Handle an event for this menu and return @True@, or return @False@
-- if the event was not handled by the menu (e.g. because it was not
-- open, or because it did not correspond to any menu entry).
handleMenuEvent :: (Eq n) => Traversal' s (Menu s n k) -> BrickEvent n e -> EventM n s Bool
handleMenuEvent which e = do
    handled <- handleMenuEventCommon which e
    if handled
       then return True
       else handleMenuEventFallback which e

handleMenuEventFallback :: (Eq n) => Traversal' s (Menu s n k) -> BrickEvent n e -> EventM n s Bool
handleMenuEventFallback which (VtyEvent (Vty.EvKey k mods)) =
    withMenu which $ \m -> do
        handled <- menuFallbackEventHandler m k mods
        when (menuIsOpen m) $ which %= closeMenu
        return handled
handleMenuEventFallback _ _ =
    return False

handleMenuEventCommon :: (Eq n) => Traversal' s (Menu s n k) -> BrickEvent n e -> EventM n s Bool
handleMenuEventCommon which (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
    withMenu which $ \m -> do
        let sel = m^.menuSelectedIndexL
        case sel of
            Nothing -> return True
            Just idx -> activateMenuItem which idx
handleMenuEventCommon which (VtyEvent (Vty.EvKey Vty.KDown [])) = do
    which %= selectNextEntry
    return True
handleMenuEventCommon which (VtyEvent (Vty.EvKey Vty.KUp [])) = do
    which %= selectPrevEntry
    return True
handleMenuEventCommon which (MouseDown n _ _ (Location (_, row))) = do
    withMenu which $ \m -> do
        let mkRegionName = m^.menuRegionNameBuilderL

        if | mkRegionName MenuTitle == n -> do
               which.menuIsOpenL %= not
               return True
           | mkRegionName MenuBody == n ->
               -- Map the location to the clicked menu entry
               activateMenuItem which row
           | otherwise -> return False
handleMenuEventCommon which (VtyEvent (Vty.EvMouseDown {})) = do
    which %= closeMenu
    return True
handleMenuEventCommon which (VtyEvent (Vty.EvKey Vty.KEsc [])) = do
    withMenu which $ \m -> do
        if menuIsOpen m
        then do
            which %= closeMenu
            return True
        else return False
handleMenuEventCommon _ _ =
    return False

-- | Activate the menu's selected entry, if any.
activateMenuItem :: Traversal' s (Menu s n k) -> Int -> EventM n s Bool
activateMenuItem which idx =
    withMenu which $ \m -> do
        s <- use id
        let handler = m^.menuEventHandlerL
            is = m^.menuItemsL
        case is V.!? idx of
            Just (MIEntry entry) -> do
                when (menuEntryEnabled entry s) $ do
                    which %= closeMenu
                    handler $ menuEntryEvent entry
                return True
            _ -> return False
