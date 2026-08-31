{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.Menu
  ( MenuItem(..)
  , MenuRegion(..)

  , Menu(..)
  , menuTitleL
  , menuItemsL
  , menuIsOpenL
  , menuWidthL
  , menuRegionNameBuilderL
  , menuSelectedIndexL

  , MenuEntry(..)
  , menuEntryTitleL
  , menuEntryEnabledL
  , menuEntryHandlerL

  , menu
  , menuSeparator
  , menuGap
  , menuEntry
  , renderMenu

  -- * Attributes
  , menuAttr
  , menuTitleAttr
  , menuTitleSelectedAttr
  , menuBodyAttr
  , menuEntryDisabledAttr
  , menuEntrySelectedAttr
  , menuEntrySelectedDisabledAttr
  )
where

import qualified Data.Text as T
import qualified Data.Vector as V

import Brick.AttrMap
import Brick.Types
import Brick.Types.EventM (EventM)
import Brick.Widgets.Border
import Brick.Widgets.Core

-- | The type of menu regions, for embedding in the application's
-- resource name and reporting mouse click events.
data MenuRegion =
    MenuTitleRegion
    -- ^ The region of a menu's title
    | MenuEntryRegion !Int
    -- ^ The region of a menu entry
    deriving (Ord, Show, Eq)

data Menu s n =
    Menu { menuTitle :: T.Text
         -- ^ The menu's title
         , menuItems :: V.Vector (MenuItem s n)
         -- ^ The contents of the menu
         , menuIsOpen :: Bool
         -- ^ Whether the menu is showing
         , menuWidth :: Int
         -- ^ The width of the menu's items within the enclosing border
         , menuRegionNameBuilder :: MenuRegion -> n
         -- ^ A function to build resource names for clickable regions
         , menuSelectedIndex :: Maybe Int
         -- ^ State for tracking the selected item index, if any
         }

data MenuItem s n =
    MISeparator
    -- ^ A horizontal border between menu items
    | MIGap
    -- ^ An empty line between menu items
    | MIEntry (MenuEntry s n)
    -- ^ A menu entry

data MenuEntry s n =
    MenuEntry { menuEntryTitle :: T.Text
              -- ^ The menu entry's title
              , menuEntryEnabled :: s -> Bool
              -- ^ The function to determine whether this menu entry is
              -- enabled
              , menuEntryHandler :: EventM n s ()
              -- ^ The event handler to invoke when this entry is
              -- activated
              }

suffixLenses ''Menu
suffixLenses ''MenuEntry

menuSeparator :: MenuItem s n
menuSeparator = MISeparator

menuGap :: MenuItem s n
menuGap = MIGap

menuEntry :: T.Text -> (s -> Bool) -> EventM n s () -> MenuItem s n
menuEntry title enabled handler =
    MIEntry $ MenuEntry title enabled handler

menu :: T.Text -> (MenuRegion -> n) -> [MenuItem s n] -> Menu s n
menu title regionNameBuilder items =
    let defaultWidth = (maximum $ menuItemWidth <$> items) + 3
    in Menu { menuTitle = title
            , menuItems = V.fromList items
            , menuIsOpen = False
            , menuWidth = defaultWidth
            , menuRegionNameBuilder = regionNameBuilder
            , menuSelectedIndex = Nothing
            }

menuItemWidth :: MenuItem s n -> Int
menuItemWidth MISeparator = 0
menuItemWidth MIGap = 0
menuItemWidth (MIEntry e) = menuEntryWidth e

menuEntryWidth :: MenuEntry s n -> Int
menuEntryWidth = textWidth . menuEntryTitle

renderMenu :: (Ord n) => s -> Menu s n -> Widget n
renderMenu s m =
    if menuIsOpen m
    then (translateLayer (Location (0, 1)) body) `above` title
    else title
    where
        setTitleAttr = if menuIsOpen m
                       then withDefAttr menuTitleSelectedAttr
                       else withDefAttr menuTitleAttr
        title = clickable (menuRegionNameBuilder m MenuTitleRegion) $
                setTitleAttr $
                txt $ menuTitle m

        body = joinBorders $
               border $
               hLimit (menuWidth m) $
               vBox $
               renderMenuItem <$> (zip [0..] $ V.toList $ menuItems m)

        renderMenuItem (_, MISeparator) = hBorder
        renderMenuItem (_, MIGap)       = vLimit 1 $ fill ' '
        renderMenuItem (i, MIEntry e)   = renderMenuEntry i e

        renderMenuEntry i e =
            clickable (menuRegionNameBuilder m (MenuEntryRegion i)) $
            setEntryAttr i e $
            vLimit 1 $
            padRight Max $
            txt $ menuEntryTitle e

        setEntryAttr i e =
            if Just i == menuSelectedIndex m
            then if menuEntryEnabled e s
                 then withDefAttr menuEntrySelectedAttr
                 else withDefAttr menuEntrySelectedDisabledAttr
            else if menuEntryEnabled e s
                 then id
                 else withDefAttr menuEntryDisabledAttr

menuAttr :: AttrName
menuAttr = attrName "brick" <> attrName "menu"

menuTitleAttr :: AttrName
menuTitleAttr = menuAttr <> attrName "title"

menuTitleSelectedAttr :: AttrName
menuTitleSelectedAttr = menuTitleAttr <> attrName "selected"

menuBodyAttr :: AttrName
menuBodyAttr = menuAttr <> attrName "body"

menuEntryDisabledAttr :: AttrName
menuEntryDisabledAttr = menuBodyAttr <> attrName "disabled"

menuEntrySelectedAttr :: AttrName
menuEntrySelectedAttr = menuBodyAttr <> attrName "selected"

menuEntrySelectedDisabledAttr :: AttrName
menuEntrySelectedDisabledAttr = menuEntrySelectedAttr <> attrName "disabled"
