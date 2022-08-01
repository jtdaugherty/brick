-- | This is the entry point into the keybinding infrastructure in
-- this library. Note that usage of this API is not required to create
-- working Brick applications; this API is provided for applications
-- that need to support custom keybindings that are less tightly coupled
-- to application behavior.
--
-- The workflow for this API is as follows:
--
-- * Create a data type @e@ with a constructor for each abstract
--   application event that you want to trigger with an input key.
-- * To each event @e@, assign a unique user-readable name (such as a
--   name you could imagine using in a configuration file to refer to
--   the event) and a list of default key bindings and use the resulting
--   data to create a 'KeyConfig'.
-- * Implement application event handlers that will be run in response
--   to either specific hard-coded keys or events @e@, both in some
--   monad @m@ of your choosing, using constructors 'onKey' and
--   'onEvent'.
-- * Use the created 'KeyConfig' and handlers to create a
--   'KeyDispatcher' with 'keyDispatcher'.
-- * As user input events arrive, dispatch them to the appropriate
--   handler using 'handleKey'.
module Brick.Keybindings.KeyDispatcher
  ( -- * Key dispatching
    KeyDispatcher
  , keyDispatcher
  , handleKey

  -- * Building handlers
  , onEvent
  , onKey

  -- * Handlers and triggers
  , Handler(..)
  , KeyHandler(..)
  , KeyEventHandler(..)
  , EventTrigger(..)

  -- * Misc
  , keyDispatcherToList
  , lookupVtyEvent
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Brick.Keybindings.KeyConfig

-- | A set of handlers for specific keys whose handlers run in the monad
-- @m@.
newtype KeyDispatcher e m = KeyDispatcher (M.Map Binding (KeyHandler e m))

-- | An 'Handler' represents a handler implementation to be invoked in
-- response to some event that runs in the monad @m@.
--
-- In general, you should never need to make one of these manually.
-- Instead, use 'onEvent' and 'onKey'. This type's internals are exposed
-- for easy inspection, not construction.
data Handler m =
    Handler { handlerDescription :: T.Text
            -- ^ The description of this handler's behavior.
            , handlerAction :: m ()
            -- ^ The action to take when this handler is invoked.
            }

-- | A handler for a specific key.
--
-- In general, you should never need to create one of these. The
-- internals are exposed to make inspection easy.
data KeyHandler e m =
    KeyHandler { khHandler :: KeyEventHandler e m
               -- ^ The handler to invoke. Note that this maintains
               -- the original abstract key event handler; this allows
               -- us to obtain the original 'EventTrigger' for the
               -- 'KeyEventHandler' upon which this 'KeyHandler'
               -- is built. This can be important for keybinding
               -- consistency checks or collision checks as well as help
               -- text generation.
               , khBinding :: Binding
               -- ^ The specific key binding that should trigger this
               -- handler.
               }

-- | Find the key handler that matches a Vty key event, if any.
--
-- This works by looking up an event handler whose binding is the
-- specified key and modifiers based on the 'KeyConfig' that was used to
-- build the 'KeyDispatcher'.
--
-- Ordinarily you will not need to use this function; use 'handleKey'
-- instead. This is provided for more direct access to the
-- 'KeyDispatcher' internals.
lookupVtyEvent :: Vty.Key -> [Vty.Modifier] -> KeyDispatcher e m -> Maybe (KeyHandler e m)
lookupVtyEvent k mods (KeyDispatcher m) = M.lookup (Binding k mods) m

-- | Handle a keyboard event by looking it up in the 'KeyDispatcher'
-- and invoking the matching binding's handler if one is found. Return
-- @True@ if the a matching handler was found and run; return @False@ if
-- no matching binding was found.
handleKey :: (Monad m)
          => KeyDispatcher e m
          -- ^ The dispatcher to use.
          -> Vty.Key
          -- ^ The key to handle.
          -> [Vty.Modifier]
          -- ^ The modifiers for the key, if any.
          -> m Bool
handleKey d k mods = do
    case lookupVtyEvent k mods d of
        Just kh -> (handlerAction $ kehHandler $ khHandler kh) >> return True
        Nothing -> return False

-- | Build a 'KeyDispatcher' to dispatch keys to handle events of type
-- @e@ using actions in a Monad @m@.
--
-- This works by taking a list of abstract 'KeyEventHandler's and
-- building a 'KeyDispatcher' of event handlers based on specific Vty
-- keys using the provided 'KeyConfig' to map between abstract key
-- events of type @e@ and Vty keys. Event handlers triggered by an event
-- @e@ are set up to be triggered by either the customized bindings for
-- @e@ in the 'KeyConfig', no bindings at all if the 'KeyConfig' has
-- marked @e@ as 'Unbound', or the default bindings for @e@ otherwise.
--
-- Once you have a 'KeyDispatcher', you can dispatch an input key event
-- to it and invoke the corresponding handler (if any) with 'handleKey'.
keyDispatcher :: (Ord e)
              => KeyConfig e
              -> [KeyEventHandler e m]
              -> KeyDispatcher e m
keyDispatcher conf ks = KeyDispatcher $ M.fromList $ buildKeyDispatcherPairs ks conf

-- | Convert a key dispatcher to a list of pairs of bindings and their
-- handlers.
keyDispatcherToList :: KeyDispatcher e m
                    -> [(Binding, KeyHandler e m)]
keyDispatcherToList (KeyDispatcher m) = M.toList m

buildKeyDispatcherPairs :: (Ord e)
                        => [KeyEventHandler e m]
                        -> KeyConfig e
                        -> [(Binding, KeyHandler e m)]
buildKeyDispatcherPairs ks conf = pairs
    where
        pairs = mkPair <$> handlers
        mkPair h = (khBinding h, h)
        handlers = concat $ keyHandlersFromConfig conf <$> ks

keyHandlersFromConfig :: (Ord e)
                      => KeyConfig e
                      -> KeyEventHandler e m
                      -> [KeyHandler e m]
keyHandlersFromConfig kc eh =
    let allBindingsFor ev | Just (BindingList ks) <- lookupKeyConfigBindings kc ev = ks
                          | Just Unbound <- lookupKeyConfigBindings kc ev = []
                          | otherwise = allDefaultBindings kc ev
        bindings = case kehEventTrigger eh of
            ByKey binding -> [binding]
            ByEvent ev    -> allBindingsFor ev
    in [ KeyHandler { khHandler = eh, khBinding = b } | b <- bindings ]

mkHandler :: T.Text -> m () -> Handler m
mkHandler msg action =
    Handler { handlerDescription = msg
            , handlerAction = action
            }

-- | Specify a handler for the specified key event.
onEvent :: e
        -- ^ The key event whose bindings should trigger this handler.
        -> T.Text
        -- ^ The description of the handler.
        -> m ()
        -- ^ The handler to invoke.
        -> KeyEventHandler e m
onEvent ev msg action =
    KeyEventHandler { kehHandler = mkHandler msg action
                    , kehEventTrigger = ByEvent ev
                    }

-- | Specify a handler for the specified key.
onKey :: (ToBinding a)
      => a
      -- ^ The binding that should trigger this handler.
      -> T.Text
      -- ^ The description of the handler.
      -> m ()
      -- ^ The handler to invoke.
      -> KeyEventHandler e m
onKey b msg action =
    KeyEventHandler { kehHandler = mkHandler msg action
                    , kehEventTrigger = ByKey $ toBinding b
                    }

-- | A trigger for an event handler.
data EventTrigger e =
    ByKey Binding
    -- ^ The key event is always triggered by a specific key.
    | ByEvent e
    -- ^ The trigger is an abstract key event.
    deriving (Show, Eq, Ord)

-- | A handler for an abstract key event.
--
-- In general, you should never need to create these manually. Instead,
-- use 'onEvent' and 'onKey'.
data KeyEventHandler e m =
    KeyEventHandler { kehHandler :: Handler m
                    -- ^ The handler to invoke.
                    , kehEventTrigger :: EventTrigger e
                    -- ^ The trigger for the handler.
                    }
