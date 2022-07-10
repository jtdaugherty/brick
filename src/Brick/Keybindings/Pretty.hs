{-# LANGUAGE OverloadedStrings #-}
module Brick.Keybindings.Pretty
  ( keybindingTextTable
  , keybindingMarkdownTable
  , keybindingHelpWidget
  )
where

import Brick
import Data.Maybe (fromJust)

import qualified Data.Text as T

import Brick.Keybindings.Parse
import Brick.Keybindings.KeyEvents
import Brick.Keybindings.KeyConfig
import Brick.Keybindings.KeyHandlerMap

data TextHunk = Verbatim T.Text
              | Comment T.Text

keybindingMarkdownTable :: (Ord e) => KeyConfig e -> [(T.Text, [KeyEventHandler e m])] -> T.Text
keybindingMarkdownTable kc sections = title <> keybindSectionStrings
    where title = "# Keybindings\n"
          keybindSectionStrings = T.concat $ sectionText <$> sections
          sectionText = mkKeybindEventSectionHelp kc keybindEventHelpMarkdown T.unlines mkHeading
          mkHeading n =
              "\n# " <> n <>
              "\n| Keybinding | Event Name | Description |" <>
              "\n| ---------- | ---------- | ----------- |"

keybindingTextTable :: (Ord e) => KeyConfig e -> [(T.Text, [KeyEventHandler e m])] -> T.Text
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
          Static binding -> (Comment "(non-customizable key)", [Verbatim $ ppBinding binding])
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
  in (label, ehDescription $ kehHandler h, evText)

keybindingHelpWidget :: (Ord e)
                     => KeyConfig e
                     -> (T.Text -> Widget n)
                     -> (T.Text, [KeyEventHandler e m])
                     -> Widget n
keybindingHelpWidget kc = mkKeybindEventSectionHelp kc keybindEventHelpWidget vBox

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
