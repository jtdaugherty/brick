{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl ((<~), (.=), (%=), use)
import Control.Monad (void)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V
import System.Environment (getArgs)
import System.Exit (exitFailure)

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
    K.keyEvents [ ("quit",      QuitEvent)
                , ("increment", IncrementEvent)
                , ("decrement", DecrementEvent)
                ]

-- | Default key bindings for each abstract key event.
defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings =
    [ (QuitEvent,      [K.ctrl 'q', K.bind V.KEsc])
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
       , _loadedCustomBindings :: Maybe FilePath
       -- ^ Set if the application found custom keybindings in the
       -- specified file.
       }

makeLenses ''St

-- | Key event handlers for our application.
handlers :: [K.KeyEventHandler KeyEvent (T.EventM () St)]
handlers =
    -- The first three handlers are triggered by keys mapped to abstract
    -- events, thus decoupling the configured key bindings from these
    -- handlers.
    [ K.onEvent QuitEvent "Quit the program"
          M.halt
    , K.onEvent IncrementEvent "Increment the counter" $
          counter %= succ
    , K.onEvent DecrementEvent "Decrement the counter" $
          counter %= subtract 1

    -- These handlers are always triggered by specific keys and thus
    -- cannot be rebound.
    , K.onKey (K.bind '\t') "Increment the counter by 10" $
          counter %= (+ 10)
    , K.onKey (K.bind V.KBackTab) "Decrement the counter by 10" $
          counter %= subtract 10
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
        binding = uncurry K.binding <$> st^.lastKey
        keybindingHelp = K.keybindingHelpWidget (st^.keyConfig) handlers
        status = hLimit 40 $
                 padRight Max $
                 vBox [ txt $ "Last key:         " <> K.ppMaybeBinding binding
                      , str $ "Last key handled: " <> show (st^.lastKeyHandled)
                      , str $ "Counter:          " <> show (st^.counter)
                      ]
        customBindingInfo =
            B.borderWithLabel (txt "Custom Bindings") $
            case st^.loadedCustomBindings of
                Nothing ->
                    hLimit 40 $
                    txtWrap $ "No custom bindings loaded. " <>
                              "Create an INI file with a " <>
                              (Text.pack $ show sectionName) <>
                              " section or use 'programs/custom_keys.ini'. " <>
                              "Pass its path to this " <>
                              "program on the command line."
                Just f -> str "Loaded custom bindings from:" <=> str (show f)
        body = C.center $
               ((padRight (Pad 7) $
                 (B.borderWithLabel (txt "Status") status) <=> customBindingInfo)
                ) <+>
                B.borderWithLabel (txt "Active Keybindings") keybindingHelp

app :: M.App St e ()
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.showFirstCursor
          }

sectionName :: Text.Text
sectionName = "keybindings"

main :: IO ()
main = do
    args <- getArgs

    -- If the command line specified the path to an INI file with custom
    -- bindings, attempt to load it.
    (customBindings, customFile) <- case args of
        [iniFilePath] -> do
            result <- K.keybindingsFromFile allKeyEvents sectionName iniFilePath
            case result of
                -- A section was found and had zero more bindings.
                Right (Just bindings) ->
                    return (bindings, Just iniFilePath)

                -- No section was found at all.
                Right Nothing -> do
                    putStrLn $ "Error: found no section " <> show sectionName <> " in " <> show iniFilePath
                    exitFailure

                -- There was some problem parsing the file as an INI
                -- file.
                Left e -> do
                    putStrLn $ "Error reading keybindings file " <> show iniFilePath <> ": " <> e
                    exitFailure

        _ -> return ([], Nothing)

    -- Create a key config that includes the default bindings as well as
    -- the custom bindings we loaded from the INI file, if any.
    let kc = K.newKeyConfig allKeyEvents defaultBindings customBindings

    void $ M.defaultMain app $ St { _keyConfig = kc
                                  , _lastKey = Nothing
                                  , _lastKeyHandled = False
                                  , _counter = 0
                                  , _loadedCustomBindings = customFile
                                  }

    -- Now demonstrate how the library's generated key binding help text
    -- looks in plain text and Markdown formats.
    let sections = [("Main", handlers)]

    putStrLn "Generated plain text help:"
    Text.putStrLn $ K.keybindingTextTable kc sections

    putStrLn "Generated Markdown help:"
    Text.putStrLn $ K.keybindingMarkdownTable kc sections
