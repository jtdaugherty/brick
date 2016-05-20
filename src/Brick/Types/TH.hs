module Brick.Types.TH
  ( suffixLenses
  )
where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Lens.Micro ((&), (.~))
import Lens.Micro.TH (DefName(..), makeLensesWith, lensRules, lensField)

-- | A template haskell function to build lenses for a record type. This
-- function differs from the 'Control.Lens.makeLenses' function in that
-- it does not require the record fields to be prefixed with underscores
-- and it adds an "L" suffix to lens names to make it clear that they
-- are lenses.
suffixLenses :: TH.Name -> TH.DecsQ
suffixLenses = makeLensesWith $
  lensRules & lensField .~ (\_ _ name -> [TopName $ TH.mkName $ TH.nameBase name ++ "L"])
