module Brick.Widgets.Menu
  ( Menu(..)
  , MenuItem(..)
  , MenuEntry(..)

  -- * Attributes
  , menuAttr
  , menuTitleAttr
  , menuTitleSelectedAttr
  , menuBodyAttr
  , menuEntrySelectedAttr
  )
where

import qualified Data.Text as T

import Brick.AttrMap
import Brick.Types
import Brick.Types.EventM (EventM)
import Brick.Widgets.Border
import Brick.Widgets.Core

data Menu s e n =
    Menu { menuTitle :: T.Text
         -- ^ The menu's title
         , menuTitleRegionName :: n
         -- ^ The menu title's clickable region name
         , menuBody :: [MenuItem s e n]
         -- ^ The contents of the menu
         , menuIsOpen :: Bool
         -- ^ Whether the menu is showing
         , menuWidth :: Maybe Int
         -- ^ If not specified, default to the widest entry
         }

data MenuItem s e n =
    MISeparator
    -- ^ A horizontal border between menu items
    | MIGap
    -- ^ An empty line between menu items
    | MIEntry (MenuEntry s e n)
    -- ^ A menu entry

data MenuEntry s e n =
    MenuEntry { menuEntryRegionName :: n
              -- ^ The menu entry's clickable region name
              , menuEntryHandler :: EventM n s ()
              -- ^ The event handler to invoke when this entry is
              -- activated
              , menuEntryTitle :: T.Text
              -- ^ The menu entry's title
              , menuEntryEnabled :: s -> Bool
              -- ^ The function to determine whether this menu entry is
              -- enabled
              }

menuAttr :: AttrName
menuAttr = attrName "brick" <> attrName "menu"

menuTitleAttr :: AttrName
menuTitleAttr = menuAttr <> attrName "title"

menuTitleSelectedAttr :: AttrName
menuTitleSelectedAttr = menuTitleAttr <> attrName "selected"

menuBodyAttr :: AttrName
menuBodyAttr = menuAttr <> attrName "body"

menuEntrySelectedAttr :: AttrName
menuEntrySelectedAttr = menuBodyAttr <> attrName "selected"
