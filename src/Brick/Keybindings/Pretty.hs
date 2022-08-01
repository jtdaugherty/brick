{-# LANGUAGE OverloadedStrings #-}
-- | This module provides functions for pretty-printing key bindings
-- and for generating Markdown, plain text, and Brick displays of event
-- handler's key binding configurations.
module Brick.Keybindings.Pretty
  ( keybindingTextTable
  , keybindingMarkdownTable
  , keybindingHelpWidget

  , ppBinding
  , ppMaybeBinding
  , ppKey
  , ppModifier
  )
where

import Brick
import Data.Maybe (fromJust)

import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Brick.Keybindings.KeyEvents
import Brick.Keybindings.KeyConfig
import Brick.Keybindings.KeyDispatcher

data TextHunk = Verbatim T.Text
              | Comment T.Text

-- | Generate a Markdown document of sections indicating the key binding
-- state for each event handler.
keybindingMarkdownTable :: (Ord e)
                        => KeyConfig e
                        -- ^ The key binding configuration in use.
                        -> [(T.Text, [KeyEventHandler e m])]
                        -- ^ Key event handlers by named section.
                        -> T.Text
keybindingMarkdownTable kc sections = title <> keybindSectionStrings
    where title = "# Keybindings\n"
          keybindSectionStrings = T.concat $ sectionText <$> sections
          sectionText = mkKeybindEventSectionHelp kc keybindEventHelpMarkdown T.unlines mkHeading
          mkHeading n =
              "\n# " <> n <>
              "\n| Keybinding | Event Name | Description |" <>
              "\n| ---------- | ---------- | ----------- |"

-- | Generate a plain text document of sections indicating the key
-- binding state for each event handler.
keybindingTextTable :: (Ord e)
                    => KeyConfig e
                    -- ^ The key binding configuration in use.
                    -> [(T.Text, [KeyEventHandler e m])]
                    -- ^ Key event handlers by named section.
                    -> T.Text
keybindingTextTable kc sections = title <> keybindSectionStrings
    where title = "Keybindings\n===========\n"
          keybindSectionStrings = T.concat $ sectionText <$> sections
          sectionText = mkKeybindEventSectionHelp kc (keybindEventHelpText keybindingWidth eventNameWidth) T.unlines mkHeading
          keybindingWidth = 15
          eventNameWidth = 30
          mkHeading n =
              "\n" <> n <>
              "\n" <> (T.replicate (T.length n) "=")

keybindEventHelpText :: Int -> Int -> (TextHunk, T.Text, [TextHunk]) -> T.Text
keybindEventHelpText width eventNameWidth (evName, desc, evs) =
    let getText (Comment s) = s
        getText (Verbatim s) = s
    in padTo width (T.intercalate ", " $ getText <$> evs) <> " " <>
       padTo eventNameWidth (getText evName) <> " " <>
       desc

padTo :: Int -> T.Text -> T.Text
padTo n s = s <> T.replicate (n - T.length s) " "

mkKeybindEventSectionHelp :: (Ord e)
                          => KeyConfig e
                          -> ((TextHunk, T.Text, [TextHunk]) -> a)
                          -> ([a] -> a)
                          -> (T.Text -> a)
                          -> (T.Text, [KeyEventHandler e m])
                          -> a
mkKeybindEventSectionHelp kc mkKeybindHelpFunc vertCat mkHeading (sectionName, kbs) =
  vertCat $ (mkHeading sectionName) :
            (mkKeybindHelpFunc <$> (mkKeybindEventHelp kc <$> kbs))

keybindEventHelpMarkdown :: (TextHunk, T.Text, [TextHunk]) -> T.Text
keybindEventHelpMarkdown (evName, desc, evs) =
    let quote s = "`" <> s <> "`"
        format (Comment s) = s
        format (Verbatim s) = quote s
        name = case evName of
            Comment s -> s
            Verbatim s -> quote s
    in "| " <> (T.intercalate ", " $ format <$> evs) <>
       " | " <> name <>
       " | " <> desc <>
       " |"

mkKeybindEventHelp :: (Ord e)
                   => KeyConfig e
                   -> KeyEventHandler e m
                   -> (TextHunk, T.Text, [TextHunk])
mkKeybindEventHelp kc h =
  let trig = kehEventTrigger h
      unbound = [Comment "(unbound)"]
      (label, evText) = case trig of
          ByKey binding ->
              (Comment "(non-customizable key)", [Verbatim $ ppBinding binding])
          ByEvent ev ->
              let name = fromJust $ keyEventName (keyConfigEvents kc) ev
              in case lookupKeyConfigBindings kc ev of
                  Nothing ->
                      if not (null (allDefaultBindings kc ev))
                      then (Verbatim name, Verbatim <$> ppBinding <$> allDefaultBindings kc ev)
                      else (Verbatim name, unbound)
                  Just Unbound ->
                      (Verbatim name, unbound)
                  Just (BindingList bs) ->
                      let result = if not (null bs)
                                   then Verbatim <$> ppBinding <$> bs
                                   else unbound
                      in (Verbatim name, result)
  in (label, handlerDescription $ kehHandler h, evText)

-- | Build a 'Widget' displaying key binding information for a single
-- related group of event handlers.
keybindingHelpWidget :: (Ord e)
                     => KeyConfig e
                     -- ^ The key binding configuration in use.
                     -> (T.Text -> Widget n)
                     -- ^ A function to render a section heading.
                     -> (T.Text, [KeyEventHandler e m])
                     -- ^ The name of the section and the list of the
                     -- event handlers.
                     -> Widget n
keybindingHelpWidget kc =
    mkKeybindEventSectionHelp kc keybindEventHelpWidget vBox

keybindEventHelpWidget :: (TextHunk, T.Text, [TextHunk]) -> Widget n
keybindEventHelpWidget (evName, desc, evs) =
    let evText = T.intercalate ", " (getText <$> evs)
        getText (Comment s) = s
        getText (Verbatim s) = s
        label = case evName of
            Comment s -> txt $ "; " <> s
            Verbatim s -> txt s -- TODO: was: emph $ txt s
    in padBottom (Pad 1) $
       vBox [ txtWrap ("; " <> desc)
            , label <+> txt (" = " <> evText)
            ]

-- | Pretty-print a 'Binding' in the same format that is parsed by
-- 'Brick.Keybindings.Parse.parseBinding'.
ppBinding :: Binding -> T.Text
ppBinding (Binding k mods) =
    T.intercalate "-" $ (ppModifier <$> mods) <> [ppKey k]

-- | Pretty-print a 'Binding' in the same format that is parsed by
-- 'Brick.Keybindings.Parse.parseBinding'; if no binding is given,
-- produce a message indicating no binding.
ppMaybeBinding :: Maybe Binding -> T.Text
ppMaybeBinding Nothing =
    "(no binding)"
ppMaybeBinding (Just b) =
    ppBinding b

-- | Pretty-print a 'Vty.Key' in the same format that is parsed by
-- 'Brick.Keybindings.Parse.parseBinding'.
ppKey :: Vty.Key -> T.Text
ppKey (Vty.KChar c)   = ppChar c
ppKey (Vty.KFun n)    = "F" <> (T.pack $ show n)
ppKey Vty.KBackTab    = "BackTab"
ppKey Vty.KEsc        = "Esc"
ppKey Vty.KBS         = "Backspace"
ppKey Vty.KEnter      = "Enter"
ppKey Vty.KUp         = "Up"
ppKey Vty.KDown       = "Down"
ppKey Vty.KLeft       = "Left"
ppKey Vty.KRight      = "Right"
ppKey Vty.KHome       = "Home"
ppKey Vty.KEnd        = "End"
ppKey Vty.KPageUp     = "PgUp"
ppKey Vty.KPageDown   = "PgDown"
ppKey Vty.KDel        = "Del"
ppKey Vty.KUpLeft     = "UpLeft"
ppKey Vty.KUpRight    = "UpRight"
ppKey Vty.KDownLeft   = "DownLeft"
ppKey Vty.KDownRight  = "DownRight"
ppKey Vty.KCenter     = "Center"
ppKey Vty.KPrtScr     = "PrintScreen"
ppKey Vty.KPause      = "Pause"
ppKey Vty.KIns        = "Insert"
ppKey Vty.KBegin      = "Begin"
ppKey Vty.KMenu       = "Menu"

-- | Pretty-print a character in the same format that is parsed by
-- 'Brick.Keybindings.Parse.parseBinding'.
ppChar :: Char -> T.Text
ppChar '\t' = "Tab"
ppChar ' '  = "Space"
ppChar c    = T.singleton c

-- | Pretty-print a 'Vty.Modifier' in the same format that is parsed by
-- 'Brick.Keybindings.Parse.parseBinding'.
ppModifier :: Vty.Modifier -> T.Text
ppModifier Vty.MMeta  = "M"
ppModifier Vty.MAlt   = "A"
ppModifier Vty.MCtrl  = "C"
ppModifier Vty.MShift = "S"
