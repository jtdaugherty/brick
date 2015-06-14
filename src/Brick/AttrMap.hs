module Brick.AttrMap
  ( AttrMap
  , AttrName
  , attrMap
  , attrMapLookup
  , setDefault
  )
where

import qualified Data.Map as M
import Data.Monoid
import Data.String (IsString(..))
import Data.Default (Default(..))

import Graphics.Vty (Attr)

data AttrName = AttrName [String]
              deriving (Show, Eq, Ord)

instance Default AttrName where
    def = mempty

instance Monoid AttrName where
    mempty = AttrName []
    mappend (AttrName as) (AttrName bs) = AttrName $ as `mappend` bs

instance IsString AttrName where
    fromString = AttrName . (:[])

data AttrMap = AttrMap Attr (M.Map AttrName Attr)
             deriving Show

instance Default AttrMap where
    def = AttrMap def mempty

attrMap :: Attr -> [(AttrName, Attr)] -> AttrMap
attrMap theDefault pairs = AttrMap theDefault (M.fromList pairs)

attrMapLookup :: AttrName -> AttrMap -> Attr
attrMapLookup (AttrName []) (AttrMap theDefault _) = theDefault
attrMapLookup n (AttrMap theDefault m) = M.findWithDefault theDefault n m

setDefault :: Attr -> AttrMap -> AttrMap
setDefault newDefault (AttrMap _ m) = AttrMap newDefault m
