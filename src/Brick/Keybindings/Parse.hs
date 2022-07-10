{-# LANGUAGE OverloadedStrings #-}
module Brick.Keybindings.Parse
  ( parseBinding
  , parseBindingList
  , ppBinding
  , ppMaybeBinding
  , ppKey
  , ppModifier
  )
where

import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Text.Read (readMaybe)

import Brick.Keybindings.KeyConfig

parseBindingList :: T.Text -> Either String BindingState
parseBindingList t =
    if T.toLower t == "unbound"
    then return Unbound
    else BindingList <$> mapM (parseBinding . T.strip) (T.splitOn "," t)

parseBinding :: T.Text -> Either String Binding
parseBinding s = go (T.splitOn "-" $ T.toLower s) []
  where go [k] mods = do
          k' <- pKey k
          return Binding { kbMods = mods, kbKey = k' }
        go (k:ks) mods = do
          m <- case k of
            "s"       -> return Vty.MShift
            "shift"   -> return Vty.MShift
            "m"       -> return Vty.MMeta
            "meta"    -> return Vty.MMeta
            "a"       -> return Vty.MAlt
            "alt"     -> return Vty.MAlt
            "c"       -> return Vty.MCtrl
            "ctrl"    -> return Vty.MCtrl
            "control" -> return Vty.MCtrl
            _         -> Left ("Unknown modifier prefix: " ++ show k)
          go ks (m:mods)
        go [] _ = Left "Empty keybinding not allowed"
        pKey "esc"       = return Vty.KEsc
        pKey "backspace" = return Vty.KBS
        pKey "enter"     = return Vty.KEnter
        pKey "left"      = return Vty.KLeft
        pKey "right"     = return Vty.KRight
        pKey "up"        = return Vty.KUp
        pKey "down"      = return Vty.KDown
        pKey "upleft"    = return Vty.KUpLeft
        pKey "upright"   = return Vty.KUpRight
        pKey "downleft"  = return Vty.KDownLeft
        pKey "downright" = return Vty.KDownRight
        pKey "center"    = return Vty.KCenter
        pKey "backtab"   = return Vty.KBackTab
        pKey "printscreen" = return Vty.KPrtScr
        pKey "pause"     = return Vty.KPause
        pKey "insert"    = return Vty.KIns
        pKey "home"      = return Vty.KHome
        pKey "pgup"      = return Vty.KPageUp
        pKey "del"       = return Vty.KDel
        pKey "end"       = return Vty.KEnd
        pKey "pgdown"    = return Vty.KPageDown
        pKey "begin"     = return Vty.KBegin
        pKey "menu"      = return Vty.KMenu
        pKey "space"     = return (Vty.KChar ' ')
        pKey "tab"       = return (Vty.KChar '\t')
        pKey t
          | Just (c, "") <- T.uncons t =
              return (Vty.KChar c)
          | Just n <- T.stripPrefix "f" t =
              case readMaybe (T.unpack n) of
                  Nothing -> Left ("Unknown keybinding: " ++ show t)
                  Just i -> return (Vty.KFun i)
          | otherwise = Left ("Unknown keybinding: " ++ show t)

ppBinding :: Binding -> T.Text
ppBinding (Binding k mods) =
    T.intercalate "-" $ (ppModifier <$> mods) <> [ppKey k]

ppMaybeBinding :: Maybe Binding -> T.Text
ppMaybeBinding Nothing =
    "(no binding)"
ppMaybeBinding (Just b) =
    ppBinding b

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

ppChar :: Char -> T.Text
ppChar '\t' = "Tab"
ppChar ' '  = "Space"
ppChar c    = T.singleton c

ppModifier :: Vty.Modifier -> T.Text
ppModifier Vty.MMeta  = "M"
ppModifier Vty.MAlt   = "A"
ppModifier Vty.MCtrl  = "C"
ppModifier Vty.MShift = "S"
