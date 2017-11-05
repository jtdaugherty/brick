{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
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
  , Next
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
import Brick.AttrMap (AttrName)

ui :: Widget n
ui =
    center $
    hLimit 40 $
    vBox $ hCenter <$>
         [ str "Press " <+> (withDefAttr keybindingAttr $ str "1") <+> str " to switch to theme 1."
         , str "Press " <+> (withDefAttr keybindingAttr $ str "2") <+> str " to switch to theme 2."
         ]

keybindingAttr :: AttrName
keybindingAttr = "keybinding"

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

appEvent :: Int -> BrickEvent () e -> EventM () (Next Int)
appEvent _ (VtyEvent (EvKey (KChar '1') [])) = continue 1
appEvent _ (VtyEvent (EvKey (KChar '2') [])) = continue 2
appEvent s (VtyEvent (EvKey (KChar 'q') [])) = halt s
appEvent s (VtyEvent (EvKey KEsc [])) = halt s
appEvent s _ = continue s

app :: App Int e ()
app =
    App { appDraw = const [ui]
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = \s ->
            -- Note that in practice this is not ideal: we don't want
            -- to build an attribute from a theme every time this is
            -- invoked, because it gets invoked once per redraw. Instead
            -- we'd build the attribute map at startup and store it in
            -- the application state. Here I just use themeToAttrMap to
            -- show the mechanics of the API.
            if s == 1
            then themeToAttrMap theme1
            else themeToAttrMap theme2
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = void $ defaultMain app 1
