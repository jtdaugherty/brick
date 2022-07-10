module Brick.Keybindings.KeyHandlerMap
  ( KeyHandlerMap
  , mkKeybindings
  , lookupVtyEvent
  , Handler(..)
  , KeyHandler(..)
  , KeyEventHandler(..)
  , EventTrigger(..)
  , onEvent
  , onKey
  , keyHandlerMapToList
  , handleKeyboardEvent
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Brick.Keybindings.KeyConfig

-- | A set of handlers for specific keys with handlers that run in the
-- monad @m@.
newtype KeyHandlerMap e m = KeyHandlerMap (M.Map Binding (KeyHandler e m))

-- | An 'Handler' represents a handler implementation to be invoked in
-- response to some event.
--
-- In general, you should never need to make one of these manually.
-- Instead, use 'onEvent' and 'onKey'.
data Handler m =
    EH { ehDescription :: T.Text
       -- ^ The description of this handler's behavior.
       , ehAction :: m ()
       -- ^ The action to take when this handler is invoked.
       }

-- | A handler for a specific key.
--
-- In general, you should never need to create one of these. The
-- internals are exposed to make inspection easy.
data KeyHandler e m =
    KH { khHandler :: KeyEventHandler e m
       -- ^ The handler to invoke. Note that this maintains the original
       -- key abstract key event handler; this allows us to obtain
       -- the original 'EventTrigger' for the 'KeyEventHandler' upon
       -- which this 'KeyHandler' is built. This can be important for
       -- keybinding consistency checks or collision checks as well as
       -- help text generation.
       , khKey :: Binding
       -- ^ The specific key that should trigger this handler.
       }

-- | Find a key handler that matches a Vty Event, if any.
lookupVtyEvent :: Vty.Event -> KeyHandlerMap e m -> Maybe (KeyHandler e m)
lookupVtyEvent (Vty.EvKey k mods) (KeyHandlerMap m) = M.lookup (Binding k mods) m
lookupVtyEvent _ _ = Nothing

-- | Handle a keyboard event by looking it up in a map of bindings and
-- invoking the matching binding's handler. Return True if the key event
-- was handled with a matching binding; False if no matching binding was
-- found (the fallback case).
handleKeyboardEvent :: (Monad m)
                    => KeyHandlerMap e m
                    -- ^ The handler map to query for a handler for this
                    -- event.
                    -> Vty.Event
                    -- ^ The event to handle.
                    -> m Bool
handleKeyboardEvent handlerMap e = do
    case lookupVtyEvent e handlerMap of
        Just kh -> (ehAction $ kehHandler $ khHandler kh) >> return True
        Nothing -> return False

-- | Build a 'KeyHandlerMap'.
--
-- This works by taking a list of abstract key event handlers and
-- building a map of event handlers based on specific Vty keys, using
-- the provided 'KeyConfig' to map between abstract key events and Vty
-- keys.
--
-- Once you have a 'KeyHandlerMap', you can dispatch a key event to it
-- and invoke the corresponding handler with 'handleKeyboardEvent'.
mkKeybindings :: (Ord e)
              => [KeyEventHandler e m]
              -> KeyConfig e
              -> KeyHandlerMap e m
mkKeybindings ks conf = KeyHandlerMap $ M.fromList $ buildKeyHandlerMapPairs ks conf

keyHandlerMapToList :: KeyHandlerMap e m
                    -> [(Binding, KeyHandler e m)]
keyHandlerMapToList (KeyHandlerMap m) = M.toList m

buildKeyHandlerMapPairs :: (Ord e)
                        => [KeyEventHandler e m]
                        -> KeyConfig e
                        -> [(Binding, KeyHandler e m)]
buildKeyHandlerMapPairs ks conf = pairs
    where
        pairs = mkPair <$> handlers
        mkPair h = (khKey h, h)
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
    in [ KH { khHandler = eh, khKey = b } | b <- bindings ]

mkHandler :: T.Text -> m () -> Handler m
mkHandler msg action =
    EH { ehDescription = msg
       , ehAction = action
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
    KEH { kehHandler = mkHandler msg action
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
    KEH { kehHandler = mkHandler msg action
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
    KEH { kehHandler :: Handler m
        -- ^ The handler to invoke.
        , kehEventTrigger :: EventTrigger e
        -- ^ The trigger for the handler.
        }
