module Brick.Types.TH
  ( suffixLenses
  , suffixLensesWith
  )
where

import qualified Language.Haskell.TH.Syntax as TH
import qualified Language.Haskell.TH.Lib as TH

import Lens.Micro ((&), (.~))
import Lens.Micro.TH (DefName(..), LensRules, makeLensesWith, lensRules, lensField)

-- | A template haskell function to build lenses for a record type. This
-- function differs from the 'Lens.Micro.TH.makeLenses' function in that
-- it does not require the record fields to be prefixed with underscores
-- and it adds an "L" suffix to lens names to make it clear that they
-- are lenses.
suffixLenses :: TH.Name -> TH.DecsQ
suffixLenses = suffixLensesWith "L" lensRules

-- | A more general version of 'suffixLenses' that allows customization
-- of the lens-building rules and allows customization of the suffix.
suffixLensesWith :: String -> LensRules -> TH.Name -> TH.DecsQ
suffixLensesWith suffix rs = makeLensesWith $
    rs & lensField .~ (\_ _ name -> [TopName $ TH.mkName $ TH.nameBase name ++ suffix])
