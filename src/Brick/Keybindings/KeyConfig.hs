module Brick.Keybindings.KeyConfig
  ( KeyConfig(keyConfigEvents)
  , Binding(..)
  , BindingState(..)
  , newKeyConfig
  , lookupKeyConfigBindings

  -- * Specifying bindings
  , ToBinding(..)
  , key
  , fn
  , char
  , meta
  , ctrl
  , shift

  -- * Querying KeyConfigs
  , firstDefaultBinding
  , firstActiveBinding
  , allDefaultBindings
  , allActiveBindings
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Graphics.Vty as Vty

import Brick.Keybindings.KeyEvents

data Binding =
    Binding { kbKey :: Vty.Key
            , kbMods :: [Vty.Modifier]
            } deriving (Eq, Show, Ord)

data BindingState =
    BindingList [Binding]
    | Unbound
    deriving (Show, Eq, Ord)

-- | A configuration of custom key bindings. A 'KeyConfig' stores
-- everything needed to resolve a key event into one or more key
-- bindings. Make a 'KeyConfig' with 'newKeyConfig', then use it to
-- dispatch to 'KeyEventHandler's with 'mkKeybindings'.
data KeyConfig e =
    KeyConfig { keyConfigBindingMap :: M.Map e BindingState
              -- ^ The map of custom bindings for events with custom
              -- bindings
              , keyConfigEvents :: KeyEvents e
              -- ^ The base mapping of events and their names that is
              -- used in this configuration
              , keyConfigDefaultBindings :: M.Map e [Binding]
              -- ^ A mapping of events and their default key bindings,
              -- if any
              }
              deriving (Show, Eq)

newKeyConfig :: (Ord e)
             => KeyEvents e
             -- ^ The base mapping of key events to use
             -> [(e, BindingState)]
             -- ^ Custom bindings by key event, such as from a
             -- configuration file. Explicitly setting an event to
             -- 'Unbound' here has the effect of disabling its default
             -- bindings.
             -> [(e, [Binding])]
             -- ^ Default bindings by key event, such as from a
             -- configuration file or embedded code
             -> KeyConfig e
newKeyConfig evs bindings defaults =
    KeyConfig { keyConfigBindingMap = M.fromList bindings
              , keyConfigEvents = evs
              , keyConfigDefaultBindings = M.fromList defaults
              }

lookupKeyConfigBindings :: (Ord e) => KeyConfig e -> e -> Maybe BindingState
lookupKeyConfigBindings kc e = M.lookup e $ keyConfigBindingMap kc

firstDefaultBinding :: (Show e, Ord e) => KeyConfig e -> e -> Maybe Binding
firstDefaultBinding kc ev = do
    bs <- M.lookup ev (keyConfigDefaultBindings kc)
    case bs of
        (b:_) -> Just b
        _ -> Nothing

allDefaultBindings :: (Ord e) => KeyConfig e -> e -> [Binding]
allDefaultBindings kc ev =
    fromMaybe [] $ M.lookup ev (keyConfigDefaultBindings kc)

firstActiveBinding :: (Show e, Ord e) => KeyConfig e -> e -> Maybe Binding
firstActiveBinding kc ev = listToMaybe $ allActiveBindings kc ev

-- | Return all active key bindings for the specified event. This
-- returns customized bindings if any have been set in the 'KeyConfig',
-- no bindings if the event has been explicitly set to 'Unbound', or the
-- default bindings if the event is absent from the customized bindings.
allActiveBindings :: (Show e, Ord e) => KeyConfig e -> e -> [Binding]
allActiveBindings kc ev = nub foundBindings
    where
        defaultBindings = allDefaultBindings kc ev
        foundBindings = case lookupKeyConfigBindings kc ev of
            Just (BindingList bs) -> bs
            Just Unbound -> []
            Nothing -> defaultBindings

-- | The class of types that can be converted into 'Binding's.
--
-- This is provided to make it easy to write and modify bindings in less
-- verbose ways.
class ToBinding a where
    -- | Binding constructor.
    toBinding :: a -> Binding

instance ToBinding Vty.Key where
    toBinding k = Binding { kbMods = [], kbKey = k }

instance ToBinding Char where
    toBinding = toBinding . Vty.KChar

instance ToBinding Binding where
    toBinding = id

-- | Add Meta to a binding.
meta :: (ToBinding a) => a -> Binding
meta val =
    let binding = toBinding val
    in binding { kbMods = Vty.MMeta : kbMods binding }

-- | Add Ctrl to a binding.
ctrl :: (ToBinding a) => a -> Binding
ctrl val =
    let binding = toBinding val
    in binding { kbMods = Vty.MCtrl : kbMods binding }

-- | Add Shift to a binding.
shift :: (ToBinding a) => a -> Binding
shift val =
    let binding = toBinding val
    in binding { kbMods = Vty.MShift : kbMods binding }

-- | Make a binding from any Vty key.
key :: Vty.Key -> Binding
key = toBinding

-- | Make a binding from any character (subject to what the keyboard can
-- actually produce).
char :: Char -> Binding
char = toBinding

-- | Function key binding.
fn :: Int -> Binding
fn = toBinding . Vty.KFun
