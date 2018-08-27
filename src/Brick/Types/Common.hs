{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Brick.Types.Common
  ( Location(..)
  , locL
  , origin
  , Edges(..)
  , eTopL, eBottomL, eRightL, eLeftL
  ) where

import Brick.Types.TH (suffixLenses)
import qualified Data.Semigroup as Sem
import GHC.Generics
import Control.DeepSeq
import Lens.Micro (_1, _2)
import Lens.Micro.Internal (Field1, Field2)

-- | A terminal screen location.
data Location = Location { loc :: (Int, Int)
                         -- ^ (Column, Row)
                         }
                deriving (Show, Eq, Ord, Read, Generic, NFData)

suffixLenses ''Location

instance Field1 Location Location Int Int where
    _1 = locL._1

instance Field2 Location Location Int Int where
    _2 = locL._2

-- | The origin (upper-left corner).
origin :: Location
origin = Location (0, 0)

instance Sem.Semigroup Location where
    (Location (w1, h1)) <> (Location (w2, h2)) = Location (w1+w2, h1+h2)

instance Monoid Location where
    mempty = origin
    mappend = (Sem.<>)

data Edges a = Edges { eTop, eBottom, eLeft, eRight :: a }
    deriving (Eq, Ord, Read, Show, Functor, Generic, NFData)

suffixLenses ''Edges

instance Applicative Edges where
    pure a = Edges a a a a
    Edges ft fb fl fr <*> Edges vt vb vl vr =
        Edges (ft vt) (fb vb) (fl vl) (fr vr)

instance Monad Edges where
    Edges vt vb vl vr >>= f = Edges
        (eTop    (f vt))
        (eBottom (f vb))
        (eLeft   (f vl))
        (eRight  (f vr))
