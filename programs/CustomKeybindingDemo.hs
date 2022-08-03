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

data KeyEvent = QuitEvent
              | IncrementEvent
              | DecrementEvent
              deriving (Ord, Eq, Show)

allKeyEvents :: K.KeyEvents KeyEvent
allKeyEvents =
    K.keyEvents [ ("quit", QuitEvent)
                , ("increment", IncrementEvent)
                , ("decrement", DecrementEvent)
                ]

defaultBindings :: [(KeyEvent, [K.Binding])]
defaultBindings =
    [ (QuitEvent, [K.ctrl 'q', K.bind V.KEsc])
    , (IncrementEvent, [K.bind '+'])
    , (DecrementEvent, [K.bind '-'])
    ]

data St =
    St { _keyConfig :: K.KeyConfig KeyEvent
       , _lastKey :: Maybe (V.Key, [V.Modifier])
       , _lastKeyHandled :: Bool
       , _counter :: Int
       }

makeLenses ''St

handlers :: [K.KeyEventHandler KeyEvent (T.EventM () St)]
handlers =
    [ K.onEvent QuitEvent "Quit the program"
          M.halt
    , K.onEvent IncrementEvent "Increment the counter" $
          counter %= succ
    , K.onEvent DecrementEvent "Decrement the counter" $
          counter %= subtract 1
    ]

appEvent :: T.BrickEvent () e -> T.EventM () St ()
appEvent (T.VtyEvent (V.EvKey k mods)) = do
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
            return $ K.Binding { K.kbKey = k
                               , K.kbMods = mods
                               }
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
    let kc = K.newKeyConfig allKeyEvents defaultBindings []
    void $ M.defaultMain app $ St { _keyConfig = kc
                                  , _lastKey = Nothing
                                  , _lastKeyHandled = False
                                  , _counter = 0
                                  }

    let sections = [("Main", handlers)]

    putStrLn "Generated plain text help:"
    Text.putStrLn $ K.keybindingTextTable kc sections

    putStrLn "Generated Markdown help:"
    Text.putStrLn $ K.keybindingMarkdownTable kc sections
