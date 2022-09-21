-- | This module is provided as a convenience to import the most
-- important parts of the API all at once. If you are new to Brick and
-- are looking to learn it, the best place to start is the
-- [Brick User Guide](https://github.com/jtdaugherty/brick/blob/master/docs/guide.rst).
-- The README also has links to other learning resources. Unlike
-- most Haskell libraries that only have API documentation, Brick
-- is best learned by reading the User Guide and other materials and
-- referring to the API docs only as needed. Enjoy!
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
