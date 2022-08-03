{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((<~), (.=), (%=), use)
import Control.Monad (void)
import qualified Data.Text.IO as Text
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.Types (Widget)
import qualified Brick.Keybindings as K
import Brick.AttrMap
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core

-- | The abstract key events for the application.
data KeyEvent = QuitEvent
              | IncrementEvent
              | DecrementEvent
              deriving (Ord, Eq, Show)

-- | The mapping of key events to their configuration field names.
allKeyEvents :: K.KeyEvents KeyEvent
allKeyEvents =
    K.keyEvents [ ("quit", QuitEvent)
                , ("increment", IncrementEvent)
                , ("decrement", DecrementEvent)
                ]

-- | Default key bindings for each abstract key event.
defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings =
    [ (QuitEvent, [K.ctrl 'q', K.bind V.KEsc])
    , (IncrementEvent, [K.bind '+'])
    , (DecrementEvent, [K.bind '-'])
    ]

data St =
    St { _keyConfig :: K.KeyConfig KeyEvent
       -- ^ The key config to use.
       , _lastKey :: Maybe (V.Key, [V.Modifier])
       -- ^ The last key that was pressed.
       , _lastKeyHandled :: Bool
       -- ^ Whether the last key was handled by a handler.
       , _counter :: Int
       -- ^ The counter value to manipulate in the handlers.
       }

makeLenses ''St

-- | Key event handlers for our application.
handlers :: [K.KeyEventHandler KeyEvent (T.EventM () St)]
handlers =
    -- The first three handlers are triggered by keys mapped to events,
    -- thus decoupling the configured key bindings from these handlers.
    [ K.onEvent QuitEvent "Quit the program"
          M.halt
    , K.onEvent IncrementEvent "Increment the counter" $
          counter %= succ
    , K.onEvent DecrementEvent "Decrement the counter" $
          counter %= subtract 1

    -- This handler is always triggered by a specific key and thus
    -- cannot be rebound to another key.
    , K.onKey (K.bind '\t') "Increment the counter by 10" $
          counter %= (+ 10)
    ]

appEvent :: T.BrickEvent () e -> T.EventM () St ()
appEvent (T.VtyEvent (V.EvKey k mods)) = do
    -- Dispatch the key to the event handler to which the key is mapped,
    -- if any. Also record in lastKeyHandled whether the dispatcher
    -- found a matching handler.
    kc <- use keyConfig
    let d = K.keyDispatcher kc handlers
    lastKey .= Just (k, mods)
    lastKeyHandled <~ K.handleKey d k mods
appEvent _ =
    return ()

drawUi :: St -> [Widget ()]
drawUi st = [body]
    where
        binding = do
            (k, mods) <- st^.lastKey
            return $ K.binding k mods
        keybindingHelp = K.keybindingHelpWidget (st^.keyConfig) handlers
        status = hLimit 40 $
                 padRight Max $
                 vBox [ txt $ "Last key:         " <> K.ppMaybeBinding binding
                      , str $ "Last key handled: " <> show (st^.lastKeyHandled)
                      , str $ "Counter:          " <> show (st^.counter)
                      ]
        body = C.center $
               (padRight (Pad 7) $ B.borderWithLabel (txt "Status") status) <+>
               B.borderWithLabel (txt "Keybinding Help") keybindingHelp

app :: M.App St e ()
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.showFirstCursor
          }

main :: IO ()
main = do
    -- Create a key config that has no customized bindings overriding
    -- the default ones.
    let kc = K.newKeyConfig allKeyEvents defaultBindings []

    void $ M.defaultMain app $ St { _keyConfig = kc
                                  , _lastKey = Nothing
                                  , _lastKeyHandled = False
                                  , _counter = 0
                                  }

    -- Now demonstrate how the library's generated key binding help text
    -- looks in plain text and Markdown formats.
    let sections = [("Main", handlers)]

    putStrLn "Generated plain text help:"
    Text.putStrLn $ K.keybindingTextTable kc sections

    putStrLn "Generated Markdown help:"
    Text.putStrLn $ K.keybindingMarkdownTable kc sections
