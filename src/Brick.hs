-- | This module is provided as a convenience to import the most
-- important parts of the API all at once. Note that the Haddock
-- documentation for this library is for /reference/ usage; if you
-- are looking for an introduction or tutorial, see the README for links
-- to plenty of material!
module Brick
  ( module Brick.Main
  , module Brick.Types
  , module Brick.Widgets.Core
  , module Brick.AttrMap
  , module Brick.Util
  )
where

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.AttrMap
import Brick.Util
