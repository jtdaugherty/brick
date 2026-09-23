{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
-- | This module provides a menu widget that is similar to the ones
-- commonly found in most graphical interface toolkits. Menus carry
-- entries that can be activated with the mouse and keyboard and invoke
-- event handlers that you specify when creating the menus and entries.
--
-- This module provides a fully general 'Menu' type and a few
-- specialized interfaces for common menu use cases:
--
-- * 'SimpleMenu': a menu with entries that have 'EventM' handlers.
--   Create one of these with 'simpleMenu'. This is a good starting point.
-- * 'DispatchingMenu': a menu whose entries correspond to abstract key
--   events bound to keys by a 'KeyDispatcher'. Create one of these with
--   'menuWithDispatcher'. This is a good choice when you already have
--   a 'KeyDispatcher' set up and would like your menu entries to be
--   triggered by the rebindable keys that trigger your dispatcher's
--   handlers.
-- * 'Menu': the fully general type for menus. Create one of these with
--   'menu'.
--
-- Menus carry a sequence of /items/, expressed by the 'MenuItem' type.
-- Items can be:
--
-- * /entries/ - named menu items that can be activated with the
--   keyboard or mouse
-- * /submenus/ - entries that correspond to nested menus
-- * /separators/ - horizontal lines dividing up groups of other
--   entries
-- * /gaps/ - vertical space between items
--
-- Menu /entries/ can be either enabled or disabled; their status
-- in this regard is determined by invoking a function of type @s
-- -> Bool@ at rendering and event-handling time. By default, all
-- entries are always enabled. Change this on a per-entry basis with
-- 'setEnabledWith'.
--
-- Depending on the type of menu you're creating, different item
-- constructors may apply. See the 'MenuItem' type aliases, since their
-- naming convention follows that of the menu types.
--
-- Handle menu events with 'handleMenuEvent', deferring to your
-- application's event handling for events that the menu bar doesn't
-- handle. To support mouse events, each menu must be identified by
-- a unique resource name; this is done by providing a resource name
-- constructor when creating each menu. The application's name type must
-- provide a constructor of type @MenuRegion -> n@ to uniquely identify
-- the menu and its constituent parts. For example, if your resource
-- name type is as follows,
--
-- @
-- data Name = Editor1 | Editor2
-- @
--
-- It would need to be modified so that a new data constructor (e.g.
-- @FileMenu@) could be given to the menu constuctors:
--
-- @
-- data Name = Editor1 | Editor2 | FileMenu MenuRegion
-- @
--
-- If you would like to use more than one menu in a menu bar
-- arrangement, see the 'Brick.Widgets.MenuBar' module, which builds on
-- this abstraction.
--
-- This API requires the use of lenses for application state fields that
-- store menu bar state.
--
-- See the @MenuDemo@ and @MenuKeybindingsDemo@ demonstration programs
-- for complete working examples of using this API.
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
  , menuEntry
  , menuSeparator
  , menuGap
  , submenu

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

import Lens.Micro.Platform ((^.), (.~), (%~), (&), Traversal', ix, each)
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

-- | The general menu type.
--
-- Menus and their items are parameterized on three types:
--
-- * @s@: the application state type used in the @App@ type,
-- * @n@: the application resource name type used in the application's
--   @Widget@ type, and
-- * @k@: the type of data carried and handled by the menu's event
--   handler when a menu entry has been activated.
--
-- Menus contain a sequence of items of type 'MenuItem'. See the
-- documentation above and the constructors for both menus and menu
-- items to create menus.
--
-- A menu is either /open/, in which case its contents are being shown
-- in a floating layer above the application's UI and it is responding
-- to events that manipulate the menu's selected entry, or it is
-- /closed/, in which case its contents are not shown and it is not
-- responding to events other than mouse clicks on its title. The menu's
-- open/closed state is affected by calls to 'openMenu', 'closeMenu',
-- and mouse click events on the menu's title.
--
-- At any given time, a menu may or may not have a currently-selected
-- entry. See 'handleMenuEvent' for details on how keyboard and mouse
-- events influence the choice and behavior of the selected entry. When
-- an entry is selected, it can be /activated/ by an @Enter@ keypress or
-- a mouse click. When activated, its event data is used to invoke the
-- menu's event handler.
--
-- To support mouse events, each menu must be identified by a unique
-- resource name; this is done by providing a resource name constructor
-- when creating each menu. The application's name type must provide a
-- constructor of type @MenuRegion -> n@ to uniquely identify the menu
-- and its constituent parts.
--
-- A menu carries an event handler that will be invoked by
-- 'handleMenuEvent' whenever a menu entry is selected.
data Menu s n k =
    Menu { menuTitle :: !T.Text
         -- ^ The menu's title
         , menuTitleRenderer :: s -> T.Text -> Widget n
         -- ^ The renderer for the menu's title
         , menuItems :: !(V.Vector (MenuItem s n k))
         -- ^ The contents of the menu
         , menuIsOpen :: !Bool
         -- ^ Whether the menu is open.
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
    | MISubmenu !(Menu s n k)
    -- ^ A submenu

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
-- This is equivalent to 'id' for non-entry items.
setEnabledWith :: (s -> Bool) -> MenuItem s n k -> MenuItem s n k
setEnabledWith f = mapMenuEntry (\e -> e { menuEntryEnabled = f })

-- | Set this menu entry's rendering function, overriding the menu's
-- default rendering behavior for this entry. This is equivalent to 'id'
-- for non-entry items.
setEntryRenderer :: (k -> T.Text -> Widget n) -> MenuItem s n k -> MenuItem s n k
setEntryRenderer f = mapMenuEntry (\e -> e { menuEntryRenderer = Just f })

-- | Set this menu's entry rendering function.
setDefaultEntryRenderer :: (k -> T.Text -> Widget n) -> Menu s n k -> Menu s n k
setDefaultEntryRenderer f m = m { menuEntryDefaultRenderer = f }

-- | Set this menu's title renderer.
setTitleRenderer :: (s -> T.Text -> Widget n) -> Menu s n k -> Menu s n k
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

-- | A submenu. The menu's title will be used as the submenu's label in
-- its parent menu.
submenu :: Menu s n k -> MenuItem s n k
submenu = MISubmenu

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
-- entry that are evaluated whenever the entries are activated. Create
-- one of these with 'simpleMenu'.
type SimpleMenu s n = Menu s n (EventM n s ())

-- | A specialization of 'MenuItem' for 'SimpleMenu'. Create these with
-- 'menuGap', 'menuSeparator', 'submenu', and 'menuEntry'.
type SimpleMenuItem s n = MenuItem s n (EventM n s ())

-- | Create a 'SimpleMenu' whose entries carry ordinary 'EventM'
-- handlers that are evaluated whenever the menu's entries are
-- activated.
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
            , menuTitleRenderer = const txt
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
-- Create one of these with 'menuWithDispatcher'.
type DispatchingMenu s n k = Menu s n (EntryTrigger s n k)

-- | A specialization of 'MenuItem' for 'DispatchingMenu'. Create these
-- with 'menuGap', 'menuSeparator', 'submenu', 'menuEntryForKey',
-- 'menuEntryForAction', and 'menuEntryForEvent'.
type DispatchingMenuItem s n k = MenuItem s n (EntryTrigger s n k)

-- | Create a 'Menu' whose entries are activated by specific triggers,
-- including specified key bindings or abstract key events associated
-- with a 'KeyDispatcher'.
--
-- To create entries in this menu, use 'menuEntryForKey',
-- 'menuEntryForEvent', and 'menuEntryForAction'.
menuWithDispatcher :: (Eq k)
                   => KeyDispatcher k (EventM n s)
                   -- ^ The key dispatcher to use to build the menu, and
                   -- whose handlers should be invoked by the menu's
                   -- entries when activated
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

-- | Close a menu and unselect any selected entry. Also closes any open
-- submenus in the menu, recursively.
closeMenu :: Menu s n k -> Menu s n k
closeMenu m =
    closeSubmenus $
        m & menuIsOpenL .~ False
          & menuSelectedIndexL .~ Nothing

closeSubmenus :: Menu s n k -> Menu s n k
closeSubmenus m =
    m & menuItemsL.each._Submenu %~ closeMenu

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
menuItemWidth (MISubmenu sm) = textWidth $ menuTitle sm

-- | Get this entry's width, i.e., the width of its label.
menuEntryWidth :: MenuEntry s n k -> Int
menuEntryWidth = textWidth . menuEntryLabel

-- | Render a menu.
--
-- If the menu is closed, only its title is rendered. If the menu is
-- open, its title is rendered with its contents shown as a floating
-- layer vertically positioned below the title.
--
-- When menu contents are shown, they are rendered in a 'border', and
-- separators are rendered with 'hBorder'. Use 'withBorderStyle' to
-- change how such borders are drawn, e.g.,
--
-- @
-- drawUi :: s -> Widget n
-- drawUi s =
--     withBorderStyle unicodeRounded $
--     renderMenu s (s^.myMenu)
-- @
renderMenu :: (Ord n) => s -> Menu s n k -> Widget n
renderMenu s m =
    if menuIsOpen m
    then (translateLayer (Location (-1, 1)) (renderMenuContents s m)) `above` title
    else title
    where
        setTitleAttr = if menuIsOpen m
                       then withDefAttr menuTitleSelectedAttr
                       else withDefAttr menuTitleAttr
        title = clickable (menuTitleName m) $
                setTitleAttr $
                menuTitleRenderer m s $
                menuTitle m

renderMenuContents :: (Ord n) => s -> Menu s n k -> Widget n
renderMenuContents s m = body
    where
        body = joinBorders $
               border $
               hLimit (menuContentWidth m) $
               clickable (menuRegionNameBuilder m MenuBody) $
               vBox $
               renderMenuItem <$> (zip [0..] $ V.toList $ menuItems m)

        renderMenuItem (_, MISeparator)  = hBorder
        renderMenuItem (_, MIGap)        = vLimit 1 $ fill ' '
        renderMenuItem (i, MIEntry e)    = renderMenuEntry i e
        renderMenuItem (i, MISubmenu sm) = renderSubmenu i sm

        renderSubmenu i sm =
            let submenuTitle = vLimit 1 $
                               padRight (Pad 1) $
                               ((padRight Max $
                                 padLeft (Pad 1) $
                                 txt $ menuTitle sm) <+> txt ">")
                layerOffset = Location (menuContentWidth m + 1, -1)
                submenuLayer = translateLayer layerOffset $ renderMenuContents s sm
                maybeAddLayer = if sm^.menuIsOpenL
                                then (submenuLayer `above`)
                                else id
                maybeSetAttr = if Just i == menuSelectedIndex m
                               then forceAttr menuEntrySelectedAttr
                               else id
            in maybeAddLayer $
               maybeSetAttr submenuTitle

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
itemIsSelectable (MISubmenu {}) = True
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

resolveMenuEventTarget :: Traversal' s (Menu s n k)
                       -> EventM n s [Int]
resolveMenuEventTarget which = do
    mMenu <- preuse which
    case mMenu of
        Nothing -> return []
        Just m ->
            case m^.menuSelectedIndexL of
                Nothing -> return []
                Just idx -> do
                    let is = m^.menuItemsL
                    case is V.!? idx of
                        Just (MISubmenu sm) -> do
                            -- If the submenu is open, recurse; if it
                            -- is not, don't add its index because we
                            -- aren't targeting the submenu at that
                            -- index.
                            if not $ sm^.menuIsOpenL
                               then return []
                               else do
                                   rest <- resolveMenuEventTarget (which.menuItemsL.ix idx._Submenu)
                                   return $ idx : rest
                        _ -> return []

targetMenu :: Traversal' s (Menu s n k)
           -> [Int]
           -> Traversal' s (Menu s n k)
targetMenu = foldl (\base idx -> base.menuItemsL.ix idx._Submenu)

-- | Handle an event for this menu and return @True@, or return @False@
-- if the event was not handled (e.g. because the event was not a menu
-- title mouse click or because the menu was not open to receive the
-- event).
--
-- Events handled include:
--
-- * Mouse clicks on the menu title will toggle whether the menu is
--   open.
-- * If a submenu entry is selected, the Right arrow key will open it
--   and the Left arrow key will close it if it is open.
-- * Mouse clicks on submenu entries will open their submenus.
-- * @Esc@ will close the menu if no submenus are open; otherwise it
--   will close the last open submenu.
-- * If no entry is selected, the Down arrow key will select the first
--   entry and the Up arrow key will select the last entry.
-- * If an entry is selected, the Down arrow key will select the next
--   entry and the Up arrow key will select the previous entry.
-- * If the selected entry is a submenu and the submenu is open, events
--   will be delegated to the submenu until it closes.
--
-- In all other cases, this will attempt to defer to the menu's selected
-- entry or submenu to handle the event. This returns @True@ if the
-- event was one of the above and was handled, @True@ if the event was
-- not one of the above but was handled by the menu's selected entry, or
-- @False@ otherwise.
--
-- A return value of @True@ indicates that the event should not be
-- handled by the application because it was destined for the menu; a
-- return value of @False@ indicates that the event should be handled by
-- the application because it did not affect the menu or its entries in
-- their current state for any reason. Consequently, a common pattern
-- when using this function will look something like this:
--
-- @
-- myApplicationEventHandler :: BrickEvent n e -> EventM n s ()
-- myApplicationEventHandler e = do
--     handled <- handleMenuEvent myMenuLens e
--     when (not handled) $ do
--         -- Go on to handle the event in the rest of the application
-- @
handleMenuEvent :: (Eq n) => Traversal' s (Menu s n k) -> BrickEvent n e -> EventM n s Bool
handleMenuEvent which e = do
    -- First, determine where we're routing the event based on whether
    -- the current selection targets an open submenu.
    path <- resolveMenuEventTarget which

    handled <- handleMenuEventCommon which path e
    if handled
       then return True
       else handleMenuEventFallback which path e

handleMenuEventFallback :: (Eq n) => Traversal' s (Menu s n k) -> [Int] -> BrickEvent n e -> EventM n s Bool
handleMenuEventFallback which path (VtyEvent (Vty.EvKey k mods)) =
    withMenu (targetMenu which path) $ \m -> do
        handled <- menuFallbackEventHandler m k mods
        return handled
handleMenuEventFallback _ _ _ =
    return False

handleMenuEventCommon :: (Eq n) => Traversal' s (Menu s n k) -> [Int] -> BrickEvent n e -> EventM n s Bool
handleMenuEventCommon which path (VtyEvent (Vty.EvKey Vty.KEnter [])) = do
    withMenu (targetMenu which path) $ \m -> do
        let sel = m^.menuSelectedIndexL
        case sel of
            Nothing -> return True
            Just idx -> activateMenuItem which path idx
handleMenuEventCommon which path (VtyEvent (Vty.EvKey Vty.KRight [])) = do
    withMenu (targetMenu which path) $ \m -> do
        let sel = m^.menuSelectedIndexL
        case sel of
            Nothing -> return False
            Just idx -> do
                -- If the selected item is a submenu that is not open,
                -- open it and select its first item.
                let is = m^.menuItemsL
                case is V.!? idx of
                    Just (MISubmenu sm) | not (sm^.menuIsOpenL) -> do
                        which.menuItemsL.ix idx._Submenu %= (selectNextEntry . openMenu)
                        return True
                    _ -> return False
handleMenuEventCommon which path (VtyEvent (Vty.EvKey Vty.KLeft [])) =
    -- Close the current menu if it is a submenu.
    case path of
        [] -> return False
        _ -> do
            targetMenu which path %= closeMenu
            return True
handleMenuEventCommon which path (VtyEvent (Vty.EvKey Vty.KDown [])) = do
    targetMenu which path %= selectNextEntry
    return True
handleMenuEventCommon which path (VtyEvent (Vty.EvKey Vty.KUp [])) = do
    targetMenu which path %= selectPrevEntry
    return True
handleMenuEventCommon which path (MouseDown n _ _ (Location (_, row))) = do
    withMenu (targetMenu which path) $ \m -> do
        let mkRegionName = m^.menuRegionNameBuilderL

        if | mkRegionName MenuTitle == n -> do
               (targetMenu which path).menuIsOpenL %= not
               return True
           | mkRegionName MenuBody == n ->
               -- Map the location to the clicked menu entry; since each
               -- item is expected to be exactly one row high, the row
               -- index here is equivalent to the item index.
               activateMenuItem which path row
           | otherwise -> return False
handleMenuEventCommon which path (VtyEvent (Vty.EvMouseDown {})) = do
    targetMenu which path %= closeMenu
    return True
handleMenuEventCommon which path (VtyEvent (Vty.EvKey Vty.KEsc [])) = do
    withMenu (targetMenu which path) $ \m -> do
        if menuIsOpen m
        then do
            targetMenu which path %= closeMenu
            return True
        else return False
handleMenuEventCommon _ _ _ =
    return False

_Submenu :: Traversal' (MenuItem s n k) (Menu s n k)
_Submenu f (MISubmenu sm) = MISubmenu <$> f sm
_Submenu _ i = pure i

-- | Activate the menu's selected entry. If the selected entry is a
-- normal entry and is enabled, trigger its handler and close the menu
-- and its ancestors. If the selected entry is a submenu, open the
-- submenu.
activateMenuItem :: Traversal' s (Menu s n k) -> [Int] -> Int -> EventM n s Bool
activateMenuItem which path idx =
    withMenu (targetMenu which path) $ \m -> do
        s <- use id
        let handler = m^.menuEventHandlerL
            is = m^.menuItemsL
        case is V.!? idx of
            Just (MIEntry entry) -> do
                when (menuEntryEnabled entry s) $ do
                    which %= closeMenu
                    handler $ menuEntryEvent entry
                return True
            Just (MISubmenu {}) -> do
                -- If the submenu entry isn't the selected one, select
                -- it.
                when (Just idx /= (m^.menuSelectedIndexL)) $
                    (targetMenu which path).menuSelectedIndexL .= Just idx

                (targetMenu which path).menuItemsL.ix idx._Submenu %= openMenu
                return True
            _ -> return False
