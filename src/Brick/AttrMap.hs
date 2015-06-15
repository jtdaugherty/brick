module Brick.AttrMap
  ( AttrMap
  , AttrName
  , attrName
  , attrMap
  , forceAttrMap
  , attrMapLookup
  , setDefault
  , applyAttrMappings
  , mergeWithDefault
  )
where

import Control.Applicative ((<$>))
import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (catMaybes)
import Data.List (inits)
import Data.String (IsString(..))
import Data.Default (Default(..))

import Graphics.Vty (Attr(..), MaybeDefault(..))

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
             | ForceAttr Attr
             deriving Show

instance Default AttrMap where
    def = AttrMap def mempty

attrName :: String -> AttrName
attrName = AttrName . (:[])

attrMap :: Attr -> [(AttrName, Attr)] -> AttrMap
attrMap theDefault pairs = AttrMap theDefault (M.fromList pairs)

forceAttrMap :: Attr -> AttrMap
forceAttrMap = ForceAttr

mergeWithDefault :: Attr -> AttrMap -> Attr
mergeWithDefault _ (ForceAttr a) = a
mergeWithDefault a (AttrMap d _) = combineAttrs d a

attrMapLookup :: AttrName -> AttrMap -> Attr
attrMapLookup _ (ForceAttr a) = a
attrMapLookup (AttrName []) (AttrMap theDefault _) = theDefault
attrMapLookup (AttrName ns) (AttrMap theDefault m) =
    let results = catMaybes $ (\n -> M.lookup n m) <$> (AttrName <$> (inits ns))
    in foldl combineAttrs theDefault results

setDefault :: Attr -> AttrMap -> AttrMap
setDefault _ (ForceAttr a) = ForceAttr a
setDefault newDefault (AttrMap _ m) = AttrMap newDefault m

combineAttrs :: Attr -> Attr -> Attr
combineAttrs (Attr s1 f1 b1) (Attr s2 f2 b2) =
    Attr (s1 `combineMDs` s2)
         (f1 `combineMDs` f2)
         (b1 `combineMDs` b2)

combineMDs :: MaybeDefault a -> MaybeDefault a -> MaybeDefault a
combineMDs _ (SetTo v) = SetTo v
combineMDs (SetTo v) _ = SetTo v
combineMDs _ v = v

applyAttrMappings :: [(AttrName, Attr)] -> AttrMap -> AttrMap
applyAttrMappings _ (ForceAttr a) = ForceAttr a
applyAttrMappings ms (AttrMap d m) = AttrMap d ((M.fromList ms) `M.union` m)
