module Main where

import Control.Monad (void)
import Control.Monad.State (put)
import Graphics.Vty
  ( white, blue, green, yellow, black, magenta
  , Event(EvKey)
  , Key(KChar, KEsc)
  )

import Brick.Main
import Brick.Themes
  ( Theme
  , newTheme
  , themeToAttrMap
  )
import Brick.Types
  ( Widget
  , BrickEvent(VtyEvent)
  , EventM
  )
import Brick.Widgets.Center
  ( hCenter
  , center
  )
import Brick.Widgets.Core
  ( (<+>)
  , vBox
  , str
  , hLimit
  , withDefAttr
  )
import Brick.Util (on, fg)
import Brick.AttrMap (AttrName, attrName)

ui :: Widget n
ui =
    center $
    hLimit 40 $
    vBox $ hCenter <$>
         [ str "Press " <+> (withDefAttr keybindingAttr $ str "1") <+> str " to switch to theme 1."
         , str "Press " <+> (withDefAttr keybindingAttr $ str "2") <+> str " to switch to theme 2."
         ]

keybindingAttr :: AttrName
keybindingAttr = attrName "keybinding"

theme1 :: Theme
theme1 =
    newTheme (white `on` blue)
             [ (keybindingAttr, fg magenta)
             ]

theme2 :: Theme
theme2 =
    newTheme (green `on` black)
             [ (keybindingAttr, fg yellow)
             ]

appEvent :: BrickEvent () e -> EventM () Int ()
appEvent (VtyEvent (EvKey (KChar '1') [])) = put 1
appEvent (VtyEvent (EvKey (KChar '2') [])) = put 2
appEvent (VtyEvent (EvKey (KChar 'q') [])) = halt
appEvent (VtyEvent (EvKey KEsc [])) = halt
appEvent _ = return ()

app :: App Int e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = \s ->
            -- Note that in practice this is not ideal: we don't want
            -- to build an attribute map from a theme every time this is
            -- invoked, because it gets invoked once per redraw. Instead
            -- we'd build the attribute map at startup and store it in
            -- the application state. Here I just use themeToAttrMap to
            -- show the mechanics of the API.
            themeToAttrMap $ if s == 1
                             then theme1
                             else theme2
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = void $ defaultMain app 1
