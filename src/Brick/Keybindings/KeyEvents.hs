-- | This module provides 'KeyEvents', a data type for mapping
-- application-defined abstract events to user-facing names (e.g.
-- for use in configuration files and documentation). This data
-- structure gives you a place to define the correspondence between
-- your application's key events and their names. A 'KeyEvents' also
-- effectively tells the key binding system about the collection of
-- possible abstract events that can be handled.
--
-- A 'KeyEvents' is used to construct a
-- 'Brick.Keybindings.KeyConfig.KeyConfig' with
-- 'Brick.Keybindings.KeyConfig.newKeyConfig'.
module Brick.Keybindings.KeyEvents
  ( KeyEvents
  , keyEvents
  , keyEventsList
  , lookupKeyEvent
  , keyEventName
  )
where

import qualified Data.Bimap as B
import qualified Data.Text as T

-- | A bidirectional mapping between events @k@ and their user-readable
-- names.
data KeyEvents k = KeyEvents (B.Bimap T.Text k)
                 deriving (Eq, Show)

-- | Build a new 'KeyEvents' map from the specified list of events and
-- names. Key event names are stored in lowercase.
--
-- Calls 'error' if any events have the same name (ignoring case) or if
-- multiple names map to the same event.
keyEvents :: (Ord k) => [(T.Text, k)] -> KeyEvents k
keyEvents pairs =
    let m = B.fromList [(T.strip $ T.toLower n, e) | (n, e) <- pairs]
    in if B.size m /= length pairs
       then error "keyEvents: input list contains duplicates by name or by event value"
       else KeyEvents $ B.fromList pairs

-- | Convert the 'KeyEvents' to a list.
keyEventsList :: KeyEvents k -> [(T.Text, k)]
keyEventsList (KeyEvents m) = B.toList m

-- | Look up the specified event name to get its abstract event. The
-- lookup ignores leading and trailing whitespace as well as case.
lookupKeyEvent :: (Ord k) => KeyEvents k -> T.Text -> Maybe k
lookupKeyEvent (KeyEvents m) name = B.lookup (T.strip $ T.toLower name) m

-- | Given an abstract event, get its event name.
keyEventName :: (Ord k) => KeyEvents k -> k -> Maybe T.Text
keyEventName (KeyEvents m) e = B.lookupR e m
