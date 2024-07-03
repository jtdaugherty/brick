module Brick.Keybindings.Normalize
  ( normalizeKey
  )
where

import Data.Char (toLower)
import qualified Graphics.Vty as Vty

-- | A keybinding involving modifiers should have its key character
-- normalized to lowercase since it's impossible to get uppercase keys
-- from the terminal when modifiers are present.
normalizeKey :: [Vty.Modifier] -> Vty.Key -> Vty.Key
normalizeKey (_:_) (Vty.KChar c) = Vty.KChar $ toLower c
normalizeKey _ k = k
