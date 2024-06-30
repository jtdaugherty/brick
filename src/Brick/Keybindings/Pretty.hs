{-# LANGUAGE OverloadedStrings #-}
-- | This module provides functions for pretty-printing key bindings
-- and for generating Markdown, plain text, and Brick displays of event
-- handler key binding configurations.
module Brick.Keybindings.Pretty
  (
  -- * Generating help output
    keybindingTextTable
  , keybindingMarkdownTable
  , keybindingHelpWidget

  -- * Pretty-printing primitives
  , ppBinding
  , ppMaybeBinding
  , ppKey
  , ppModifier

  -- * Attributes for Widget rendering
  , keybindingHelpBaseAttr
  , eventNameAttr
  , eventDescriptionAttr
  , keybindingAttr
  )
where

import Brick
import Data.List (sort, intersperse)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Graphics.Vty as Vty

import Brick.Keybindings.KeyEvents
import Brick.Keybindings.KeyConfig
import Brick.Keybindings.KeyDispatcher

data TextHunk = Verbatim T.Text
              | Comment T.Text

-- | Generate a Markdown document of sections indicating the key binding
-- state for each event handler.
keybindingMarkdownTable :: (Ord k)
                        => KeyConfig k
                        -- ^ The key binding configuration in use.
                        -> [(T.Text, [KeyEventHandler k m])]
                        -- ^ Key event handlers by named section.
                        -> T.Text
keybindingMarkdownTable kc sections = title <> keybindSectionStrings
    where title = "# Keybindings\n"
          keybindSectionStrings = T.concat $ sectionText <$> sections
          sectionText (heading, handlers) =
              mkHeading heading <>
              mkKeybindEventSectionHelp kc keybindEventHelpMarkdown T.unlines handlers
          mkHeading n =
              "\n# " <> n <>
              "\n| Keybinding | Event Name | Description |" <>
              "\n| ---------- | ---------- | ----------- |\n"

-- | Generate a plain text document of sections indicating the key
-- binding state for each event handler.
keybindingTextTable :: (Ord k)
                    => KeyConfig k
                    -- ^ The key binding configuration in use.
                    -> [(T.Text, [KeyEventHandler k m])]
                    -- ^ Key event handlers by named section.
                    -> T.Text
keybindingTextTable kc sections = title <> keybindSectionStrings
    where title = "Keybindings\n===========\n"
          keybindSectionStrings = T.concat $ sectionText <$> sections
          sectionText (heading, handlers) =
              mkHeading heading <>
              mkKeybindEventSectionHelp kc (keybindEventHelpText keybindingWidth eventNameWidth) T.unlines handlers
          keybindingWidth = 15
          eventNameWidth = 30
          mkHeading n =
              "\n" <> n <>
              "\n" <> (T.replicate (T.length n) "=") <>
              "\n"

keybindEventHelpText :: Int -> Int -> (TextHunk, T.Text, [TextHunk]) -> T.Text
keybindEventHelpText width eventNameWidth (evName, desc, evs) =
    let getText (Comment s) = s
        getText (Verbatim s) = s
    in padTo width (T.intercalate ", " $ getText <$> evs) <> " " <>
       padTo eventNameWidth (getText evName) <> " " <>
       desc

padTo :: Int -> T.Text -> T.Text
padTo n s = s <> T.replicate (n - T.length s) " "

mkKeybindEventSectionHelp :: (Ord k)
                          => KeyConfig k
                          -> ((TextHunk, T.Text, [TextHunk]) -> a)
                          -> ([a] -> a)
                          -> [KeyEventHandler k m]
                          -> a
mkKeybindEventSectionHelp kc mkKeybindHelpFunc vertCat kbs =
  vertCat $ mkKeybindHelpFunc <$> (mkKeybindEventHelp kc <$> kbs)

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

mkKeybindEventHelp :: (Ord k)
                   => KeyConfig k
                   -> KeyEventHandler k m
                   -> (TextHunk, T.Text, [TextHunk])
mkKeybindEventHelp kc h =
  let trig = kehEventTrigger h
      unbound = [Comment "(unbound)"]
      (label, evText) = case trig of
          ByKey b ->
              (Comment "(non-customizable key)", [Verbatim $ ppBinding b])
          ByEvent ev ->
              let name = maybe (Comment "(unnamed)") Verbatim $ keyEventName (keyConfigEvents kc) ev
              in case lookupKeyConfigBindings kc ev of
                  Nothing ->
                      if not (null (allDefaultBindings kc ev))
                      then (name, Verbatim <$> ppBinding <$> allDefaultBindings kc ev)
                      else (name, unbound)
                  Just Unbound ->
                      (name, unbound)
                  Just (BindingList bs) ->
                      let result = if not (null bs)
                                   then Verbatim <$> ppBinding <$> bs
                                   else unbound
                      in (name, result)
  in (label, handlerDescription $ kehHandler h, evText)

-- | Build a 'Widget' displaying key binding information for a single
-- related group of event handlers. This is provided for convenience
-- so that basic help text for the application's event handlers can be
-- produced and embedded in the UI.
--
-- The resulting widget lists the key events (and keys) bound to the
-- specified handlers, along with the events' names and the list of
-- available key bindings for each handler.
keybindingHelpWidget :: (Ord k)
                     => KeyConfig k
                     -- ^ The key binding configuration in use.
                     -> [KeyEventHandler k m]
                     -- ^ The list of the event handlers to include in
                     -- the help display.
                     -> Widget n
keybindingHelpWidget kc =
    withDefAttr keybindingHelpBaseAttr .
    mkKeybindEventSectionHelp kc keybindEventHelpWidget (vBox . intersperse (str " "))

keybindEventHelpWidget :: (TextHunk, T.Text, [TextHunk]) -> Widget n
keybindEventHelpWidget (evName, desc, evs) =
    let evText = T.intercalate ", " (getText <$> evs)
        getText (Comment s) = s
        getText (Verbatim s) = s
        label = withDefAttr eventNameAttr $ case evName of
            Comment s -> txt s
            Verbatim s -> txt s
    in vBox [ withDefAttr eventDescriptionAttr $ txt desc
            , label <+> txt " = " <+> withDefAttr keybindingAttr (txt evText)
            ]

-- | Pretty-print a 'Binding' in the same format that is parsed by
-- 'Brick.Keybindings.Parse.parseBinding'.
ppBinding :: Binding -> T.Text
ppBinding (Binding k mods) =
    T.intercalate "-" $ (ppModifier <$> modifierList mods) <> [ppKey k]

modifierList :: S.Set Vty.Modifier -> [Vty.Modifier]
modifierList = sort . S.toList

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

-- | The base attribute for 'Widget' keybinding help.
keybindingHelpBaseAttr :: AttrName
keybindingHelpBaseAttr = attrName "keybindingHelp"

-- | The attribute for event names in keybinding help 'Widget's.
eventNameAttr :: AttrName
eventNameAttr = keybindingHelpBaseAttr <> attrName "eventName"

-- | The attribute for event descriptions in keybinding help 'Widget's.
eventDescriptionAttr :: AttrName
eventDescriptionAttr = keybindingHelpBaseAttr <> attrName "eventDescription"

-- | The attribute for keybinding lists in keybinding help 'Widget's.
keybindingAttr :: AttrName
keybindingAttr = keybindingHelpBaseAttr <> attrName "keybinding"
