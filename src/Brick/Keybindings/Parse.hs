{-# LANGUAGE OverloadedStrings #-}
-- | This module provides key binding string parsing functions for use
-- in e.g. reading key bindings from configuration files.
module Brick.Keybindings.Parse
  ( parseBinding
  , parseBindingList
  )
where

import qualified Data.Text as T
import qualified Graphics.Vty as Vty
import Text.Read (readMaybe)

import Brick.Keybindings.KeyConfig

-- | Parse a key binding list into a 'BindingState'.
--
-- A key binding list either the string @"unbound"@ or is a
-- comma-separated list of 'Binding's parsed with 'parseBinding'.
parseBindingList :: T.Text -> Either String BindingState
parseBindingList t =
    if T.toLower t == "unbound"
    then return Unbound
    else BindingList <$> mapM (parseBinding . T.strip) (T.splitOn "," $ T.strip t)

-- | Parse a key binding string. Key binding strings specify zero or
-- more modifier keys and a base key, separated by hyphens.
--
-- @
-- (modifier "-")* key
-- @
--
-- e.g. @c-down@, @backspace@, @ctrl-shift-f1@.
--
-- where each @modifier@ is parsed case-insensitively as follows:
--
-- * @"s", "shift"@: 'Vty.MShift'
-- * @"m", "meta"@: 'Vty.MMeta'
-- * @"a", "alt"@: 'Vty.MAlt'
-- * @"c", "ctrl", "control"@: 'Vty.MCtrl'
--
-- and @key@ is parsed case-insensitively as follows:
--
-- * "f1", "f2", ...: 'Vty.KFun'
-- * "esc": 'Vty.KEsc'
-- * "backspace": 'Vty.KBS'
-- * "enter": 'Vty.KEnter'
-- * "left": 'Vty.KLeft'
-- * "right": 'Vty.KRight'
-- * "up": 'Vty.KUp'
-- * "down": 'Vty.KDown'
-- * "upleft": 'Vty.KUpLeft'
-- * "upright": 'Vty.KUpRight'
-- * "downleft": 'Vty.KDownLeft'
-- * "downright": 'Vty.KDownRight'
-- * "center": 'Vty.KCenter'
-- * "backtab": 'Vty.KBackTab'
-- * "printscreen": 'Vty.KPrtScr'
-- * "pause": 'Vty.KPause'
-- * "insert": 'Vty.KIns'
-- * "home": 'Vty.KHome'
-- * "pgup": 'Vty.KPageUp'
-- * "del": 'Vty.KDel'
-- * "end": 'Vty.KEnd'
-- * "pgdown": 'Vty.KPageDown'
-- * "begin": 'Vty.KBegin'
-- * "menu": 'Vty.KMenu'
-- * "space": @' '@
-- * "tab": @'\\t'@
-- * Otherwise, 'Vty.KChar'
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
          | T.length t == 1 =
              return (Vty.KChar $ T.last s)
          | Just n <- T.stripPrefix "f" t =
              case readMaybe (T.unpack n) of
                  Nothing -> Left ("Unknown keybinding: " ++ show t)
                  Just i -> return (Vty.KFun i)
          | otherwise = Left ("Unknown keybinding: " ++ show t)
