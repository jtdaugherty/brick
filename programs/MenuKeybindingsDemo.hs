{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void, forM_, when)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Graphics.Vty as V
import System.Environment (getArgs)
import System.Exit (exitFailure)

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget)
import qualified Brick.Main as M
import Brick.Widgets.Core (txtWrap, hLimit, padLeft, Padding(..))
import Brick.Widgets.Center (center)
import Brick.Widgets.Menu

import qualified Brick.Keybindings as K

-- | The abstract key events for the application.
data KeyEvent = QuitEvent
              | ToggleFileMenuEvent
              | NewEvent
              | OpenEvent
              deriving (Ord, Eq, Show)

-- | The mapping of key events to their configuration field names.
allKeyEvents :: K.KeyEvents KeyEvent
allKeyEvents =
    K.keyEvents [ ("quit",             QuitEvent)
                , ("toggle-file-menu", ToggleFileMenuEvent)
                , ("new",              NewEvent)
                , ("open",             OpenEvent)
                ]

-- | Default key bindings for each abstract key event.
defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings =
    [ (QuitEvent,           [K.ctrl 'q'])
    , (ToggleFileMenuEvent, [K.meta 'f'])
    , (NewEvent,            [K.meta 'n'])
    , (OpenEvent,           [K.meta 'o'])
    ]

data Name = FileMenu MenuRegion
          deriving (Show, Ord, Eq)

data St =
    St { _keyConfig :: K.KeyConfig KeyEvent
       , _dispatcher :: K.KeyDispatcher KeyEvent (T.EventM Name St)
       , _fileMenu :: Menu St Name (K.EventTrigger KeyEvent)
       , _lastAction :: Text.Text
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ padLeft (Pad 1) $
      renderMenu st (st^.fileMenu)
    , center $
      hLimit 40 $
      txtWrap $
      Text.unlines $
      [ "Click the menu title with the mouse or press Alt-F to open the menu."
      , ""
      , "When the menu is open, press arrow keys to select items and then " <>
        "press Enter to activate them, or click them with the mouse instead."
      , ""
      , "When the menu is open or closed, press the keybindings shown in the " <>
        "menu to activate the corresponding menu items."
      , ""
      , "Last action: " <> st^.lastAction
      ]
    ]

-- | Key event handlers for our application.
handlers :: [K.KeyEventHandler KeyEvent (T.EventM n St)]
handlers =
    [ K.onEvent QuitEvent "Quit the program" M.halt

    , K.onEvent ToggleFileMenuEvent "Toggle the File menu" $ do
        lastAction .= "Toggled the File menu"
        fileMenu %= toggleMenu

    , K.onEvent NewEvent "New" $
        lastAction .= "Activated New... menu entry"

    , K.onEvent OpenEvent "Open" $
        lastAction .= "Activated Open... menu entry"
    ]

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent e = void $ handleMenuEvent fileMenu e

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (menuAttr, fg V.white)
    , (menuTitleAttr, fg V.white)
    , (menuTitleSelectedAttr, V.black `on` V.white)
    , (menuEntryDisabledAttr, fg V.red)
    , (menuEntrySelectedAttr, V.black `on` V.yellow)
    , (menuEntrySelectedDisabledAttr, V.black `on` V.red)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
          }

fileMenuState :: K.KeyDispatcher KeyEvent (T.EventM Name St) -> Menu St Name (K.EventTrigger KeyEvent)
fileMenuState d =
    menuWithDispatcher d "File" FileMenu
        [ menuEntryForEvent "New..." (const True) NewEvent
        , menuEntryForEvent "Open..." (const True) OpenEvent
        , menuSeparator
        , menuEntryForEvent "Exit" (const True) QuitEvent
        ]

sectionName :: Text.Text
sectionName = "keybindings"

main :: IO ()
main = do
    args <- getArgs

    -- If the command line specified the path to an INI file with custom
    -- bindings, attempt to load it.
    (customBindings, _) <- case args of
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

    -- Before starting the application, check on whether any events have
    -- colliding bindings. Exit if so.
    --
    -- Note that in a Real Application, we would more than likely
    -- want to check for collisions among specific sets of
    -- events. For example, if 'Esc' was bound to both 'quit' and
    -- 'close-dialog-box', we might not care about such a collision
    -- if the application only ever handled the 'close-dialog-box'
    -- event in a separate mode and only ever handled 'quit' at the
    -- top-level of the event handler. But if we had two events such as
    -- 'dialog-box-okay' and 'dialog-box-cancel' that were intended to
    -- be handled in the same mode, we might want to check that those
    -- two events did not have the same binding.
    forM_ (K.keyEventMappings kc) $ \(b, evs) -> do
        when (S.size evs > 1) $ do
            Text.putStrLn $ "Error: key '" <> K.ppBinding b <> "' is bound to multiple events:"
            forM_ evs $ \e ->
                Text.putStrLn $ "  " <> Text.pack (show e) <> " (" <> fromJust (K.keyEventName allKeyEvents e) <> ")"
            exitFailure

    -- Now build a key dispatcher for our event handlers. If this fails
    -- due to key collision detection, we'll print out info about the
    -- collisions.
    d <- case K.keyDispatcher kc handlers of
        Right d -> return d
        Left collisions -> do
            putStrLn "Error: some key events have the same keys bound to them."

            forM_ collisions $ \(b, hs) -> do
                Text.putStrLn $ "Handlers with the '" <> K.ppBinding b <> "' binding:"
                forM_ hs $ \h -> do
                    let trigger = case K.kehEventTrigger $ K.khHandler h of
                            K.ByKey k   -> "triggered by the key '" <> K.ppBinding k <> "'"
                            K.ByEvent e -> "triggered by the event '" <> fromJust (K.keyEventName allKeyEvents e) <> "'"
                        desc = K.handlerDescription $ K.kehHandler $ K.khHandler h

                    Text.putStrLn $ "  " <> desc <> " (" <> trigger <> ")"

            exitFailure

    void $ M.defaultMain app $ St kc d (fileMenuState d) "(none yet)"
