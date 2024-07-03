-- | This module provides 'KeyConfig' and associated functions. A
-- 'KeyConfig' is the basis for the custom keybinding system in this
-- library.
--
-- To get started, see 'newKeyConfig'. Once a 'KeyConfig' has been
-- constructed, see 'Brick.Keybindings.KeyHandlerMap.keyDispatcher'.
--
-- Since a key configuration can have keys bound to multiple events, it
-- is the application author's responsibility to check for collisions
-- since the nature of the collisions will depend on how the application
-- is implemented. To check for collisions, use the result of
-- 'keyEventMappings'.
module Brick.Keybindings.KeyConfig
  ( KeyConfig
  , newKeyConfig
  , BindingState(..)

  -- * Specifying bindings
  , Binding(..)
  , ToBinding(..)
  , binding
  , fn
  , meta
  , ctrl
  , shift

  -- * Querying KeyConfigs
  , firstDefaultBinding
  , firstActiveBinding
  , allDefaultBindings
  , allActiveBindings
  , keyEventMappings

  -- * Misc
  , keyConfigEvents
  , lookupKeyConfigBindings
  )
where

import Data.List (nub)
import qualified Data.Map.Strict as M
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Set as S
import Data.Maybe (fromMaybe, listToMaybe, catMaybes)
import qualified Graphics.Vty as Vty

import Brick.Keybindings.KeyEvents
import Brick.Keybindings.Normalize

-- | A key binding.
--
-- The easiest way to express 'Binding's is to use the helper functions
-- in this module that work with instances of 'ToBinding', e.g.
--
-- @
-- let ctrlB = 'ctrl' \'b\'
--     shiftX = 'shift' \'x\'
--     ctrlMetaK = 'ctrl' $ 'meta' \'k\'
--     -- Or with Vty keys directly:
--     ctrlDown = 'ctrl' 'Graphics.Vty.Input.KDown'
-- @
data Binding =
    Binding { kbKey :: Vty.Key
            -- ^ The key itself.
            , kbMods :: S.Set Vty.Modifier
            -- ^ The set of modifiers.
            } deriving (Eq, Show, Ord)

-- | Construct a 'Binding'. Modifier order is ignored. If modifiers
-- are given and the binding is for a character key, it is forced to
-- lowercase.
binding :: Vty.Key -> [Vty.Modifier] -> Binding
binding k mods =
    Binding { kbKey = normalizeKey mods k
            , kbMods = S.fromList mods
            }

-- | An explicit configuration of key bindings for a key event.
data BindingState =
    BindingList [Binding]
    -- ^ Bind the event to the specified list of bindings.
    | Unbound
    -- ^ Disable all bindings for the event, including default bindings.
    deriving (Show, Eq, Ord)

-- | A configuration of custom key bindings. A 'KeyConfig'
-- stores everything needed to resolve a key event into one or
-- more key bindings. Make a 'KeyConfig' with 'newKeyConfig',
-- then use it to dispatch to 'KeyEventHandler's with
-- 'Brick.Keybindings.KeyHandlerMap.keyDispatcher'.
--
-- Make a new 'KeyConfig' with 'newKeyConfig'.
--
-- A 'KeyConfig' stores:
--
-- * A collection of named key events, mapping the event type @k@ to
--   'Text' labels.
-- * For each event @k@, optionally store a list of default key bindings
--   for that event.
-- * An optional customized binding list for each event, setting the
--   event to either 'Unbound' or providing explicit overridden bindings
--   with 'BindingList'.
data KeyConfig k =
    KeyConfig { keyConfigCustomBindings :: [(k, BindingState)]
              -- ^ The list of custom binding states for events with
              -- custom bindings. We use a list to ensure that we
              -- preserve key bindings for keys that are mapped to more
              -- than one event. This may be valid or invalid depending
              -- on the events in question; whether those bindings
              -- constitute a collision is up to the application
              -- developer to check.
              , keyConfigEvents :: KeyEvents k
              -- ^ The base mapping of events and their names that is
              -- used in this configuration.
              , keyConfigDefaultBindings :: M.Map k [Binding]
              -- ^ A mapping of events and their default key bindings,
              -- if any.
              }
              deriving (Show, Eq)

-- | Build a 'KeyConfig' with the specified 'KeyEvents' event-to-name
-- mapping, list of default bindings by event, and list of custom
-- bindings by event.
newKeyConfig :: (Ord k)
             => KeyEvents k
             -- ^ The base mapping of key events and names to use.
             -> [(k, [Binding])]
             -- ^ Default bindings by key event, such as from a
             -- configuration file or embedded code. Optional on a
             -- per-event basis.
             -> [(k, BindingState)]
             -- ^ Custom bindings by key event, such as from a
             -- configuration file. Explicitly setting an event to
             -- 'Unbound' here has the effect of disabling its default
             -- bindings. Optional on a per-event basis. Note that this
             -- function does not check for collisions since it is up to
             -- the application to determine whether a key bound to more
             -- than one event constitutes a collision!
             -> KeyConfig k
newKeyConfig evs defaults bindings =
    KeyConfig { keyConfigCustomBindings = bindings
              , keyConfigEvents = evs
              , keyConfigDefaultBindings = M.fromList defaults
              }

-- | Return a list of mappings including each key bound to any event
-- combined with the list of events to which it is bound. This is useful
-- for identifying problematic key binding collisions. Since key binding
-- collisions cannot be determined in general, we leave it up to the
-- application author to determine which key-to-event bindings are
-- problematic.
keyEventMappings :: (Ord k, Eq k) => KeyConfig k -> [(Binding, S.Set k)]
keyEventMappings kc = M.toList resultMap
    where
        -- Get all default bindings
        defaultBindings = M.toList $ keyConfigDefaultBindings kc
        -- Get all explicitly unbound events
        explicitlyUnboundEvents = fmap fst $ filter ((== Unbound) . snd) $ keyConfigCustomBindings kc
        -- Remove explicitly unbound events from the default set of
        -- bindings
        defaultBindingsWithoutUnbound = filter ((`notElem` explicitlyUnboundEvents) . fst) defaultBindings
        -- Now get customized binding lists
        customizedKeybindingLists = catMaybes $ (flip fmap) (keyConfigCustomBindings kc) $ \(k, bState) -> do
            case bState of
                Unbound -> Nothing
                BindingList bs -> Just (k, bs)
        -- Now build a map from binding to event list
        allPairs = defaultBindingsWithoutUnbound <>
                   customizedKeybindingLists
        addBindings m (ev, bs) =
            M.unionWith S.union m $ M.fromList [(b, S.singleton ev) | b <- bs]
        resultMap = foldl addBindings mempty allPairs

-- | Look up the binding state for the specified event. This returns
-- 'Nothing' when the event has no explicitly configured custom
-- 'BindingState'.
lookupKeyConfigBindings :: (Ord k) => KeyConfig k -> k -> Maybe BindingState
lookupKeyConfigBindings kc e = lookup e $ keyConfigCustomBindings kc

-- | A convenience function to return the first result of
-- 'allDefaultBindings', if any.
firstDefaultBinding :: (Show k, Ord k) => KeyConfig k -> k -> Maybe Binding
firstDefaultBinding kc ev = do
    bs <- M.lookup ev (keyConfigDefaultBindings kc)
    case bs of
        (b:_) -> Just b
        _ -> Nothing

-- | Returns the list of default bindings for the specified event,
-- irrespective of whether the event has been explicitly configured with
-- other bindings or set to 'Unbound'.
allDefaultBindings :: (Ord k) => KeyConfig k -> k -> [Binding]
allDefaultBindings kc ev =
    fromMaybe [] $ M.lookup ev (keyConfigDefaultBindings kc)

-- | A convenience function to return the first result of
-- 'allActiveBindings', if any.
firstActiveBinding :: (Show k, Ord k) => KeyConfig k -> k -> Maybe Binding
firstActiveBinding kc ev = listToMaybe $ allActiveBindings kc ev

-- | Return all active key bindings for the specified event. This
-- returns customized bindings if any have been set in the 'KeyConfig',
-- no bindings if the event has been explicitly set to 'Unbound', or the
-- default bindings if the event is absent from the customized bindings.
allActiveBindings :: (Show k, Ord k) => KeyConfig k -> k -> [Binding]
allActiveBindings kc ev = nub foundBindings
    where
        defaultBindings = allDefaultBindings kc ev
        foundBindings = case lookupKeyConfigBindings kc ev of
            Just (BindingList bs) -> bs
            Just Unbound -> []
            Nothing -> defaultBindings

-- | The class of types that can form the basis of 'Binding's.
--
-- This is provided to make it easy to write and modify bindings in less
-- verbose ways.
class ToBinding a where
    -- | Binding constructor.
    bind :: a -> Binding

instance ToBinding Vty.Key where
    bind k = Binding { kbMods = mempty, kbKey = k }

instance ToBinding Char where
    bind = bind . Vty.KChar

instance ToBinding Binding where
    bind = id

addModifier :: (ToBinding a) => Vty.Modifier -> a -> Binding
addModifier m val =
    let b = bind val
        newMods = S.insert m $ kbMods b
    in b { kbMods = newMods
         , kbKey = normalizeKey (S.toList newMods) $ kbKey b
         }

-- | Add Meta to a binding. If the binding is for a character key, force
-- it to lowercase.
meta :: (ToBinding a) => a -> Binding
meta = addModifier Vty.MMeta

-- | Add Ctrl to a binding. If the binding is for a character key, force
-- it to lowercase.
ctrl :: (ToBinding a) => a -> Binding
ctrl = addModifier Vty.MCtrl

-- | Add Shift to a binding. If the binding is for a character key, force
-- it to lowercase.
shift :: (ToBinding a) => a -> Binding
shift = addModifier Vty.MShift

-- | Function key binding.
fn :: Int -> Binding
fn = bind . Vty.KFun
