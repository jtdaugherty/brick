-- | The re-exporting catch-all module for the customizable keybindings
-- API.
--
-- To get started using this API, see the documentation in
-- @KeyDispatcher@ as well as the User Guide section on customizable
-- keybindings.
module Brick.Keybindings
  ( module Brick.Keybindings.KeyEvents
  , module Brick.Keybindings.KeyConfig
  , module Brick.Keybindings.KeyDispatcher
  , module Brick.Keybindings.Pretty
  , module Brick.Keybindings.Parse
  )
where

import Brick.Keybindings.KeyEvents
import Brick.Keybindings.KeyConfig
import Brick.Keybindings.KeyDispatcher
import Brick.Keybindings.Pretty
import Brick.Keybindings.Parse
