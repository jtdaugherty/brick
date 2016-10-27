-- | This module provides types and functions for managing an attribute
-- map which maps attribute names ('AttrName') to attributes ('Attr').
-- This module is designed to be used with the 'OverloadedStrings'
-- language extension to permit easy construction of 'AttrName' values
-- and you should also use 'mappend' ('<>') to combine names.
--
-- Attribute maps work by mapping hierarchical attribute names to
-- attributes and inheriting parent names' attributes when child names
-- specify partial attributes. Hierarchical names are created with 'mappend':
--
-- @
-- let n = attrName "parent" <> attrName "child"
-- @
--
-- Attribute names are mapped to attributes, but some attributes may
-- be partial (specify only a foreground or background color). When
-- attribute name lookups occur, the attribute corresponding to a more
-- specific name ('parent <> child' as above) is sucessively merged with
-- the parent attribute ('parent' as above) all the way to the "root"
-- of the attribute map, the map's default attribute. In this way, more
-- specific attributes inherit what they don't specify from more general
-- attributes in the same hierarchy. This allows more modularity and
-- less repetition in specifying how elements of your user interface
-- take on different attributes.
module Brick.AttrMap
  ( AttrMap
  , AttrName
  -- * Construction
  , attrMap
  , forceAttrMap
  , attrName
  -- * Finding attributes from names
  , attrMapLookup
  -- * Manipulating attribute maps
  , setDefault
  , applyAttrMappings
  , mergeWithDefault
  , mapAttrName
  , mapAttrNames
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid
#endif

import qualified Data.Map as M
import Data.Maybe (catMaybes)
import Data.List (inits)
import Data.String (IsString(..))
import Data.Default (Default(..))

import Graphics.Vty (Attr(..), MaybeDefault(..))

-- | An attribute name. Attribute names are hierarchical; use 'mappend'
-- ('<>') to assemble them. Hierachy in an attribute name is used to
-- represent increasing levels of specificity in referring to the
-- attribute you want to use for a visual element, with names to the
-- left being general and names to the right being more specific. For
-- example:
--
-- @
-- "window" <> "border"
-- "window" <> "title"
-- "header" <> "clock" <> "seconds"
-- @
data AttrName = AttrName [String]
              deriving (Show, Eq, Ord)

instance Default AttrName where
    def = mempty

instance Monoid AttrName where
    mempty = AttrName []
    mappend (AttrName as) (AttrName bs) = AttrName $ as `mappend` bs

instance IsString AttrName where
    fromString = AttrName . (:[])

-- | An attribute map which maps 'AttrName' values to 'Attr' values.
data AttrMap = AttrMap Attr (M.Map AttrName Attr)
             | ForceAttr Attr
             deriving Show

instance Default AttrMap where
    def = AttrMap def mempty

-- | Create an attribute name from a string.
attrName :: String -> AttrName
attrName = AttrName . (:[])

-- | Create an attribute map.
attrMap :: Attr
        -- ^ The map's default attribute to be returned when a name
        -- lookup fails, and the attribute that will be merged with
        -- successful lookups.
        -> [(AttrName, Attr)]
        -- ^ The map's initial contents.
        -> AttrMap
attrMap theDefault pairs = AttrMap theDefault (M.fromList pairs)

-- | Create an attribute map in which all lookups map to the same
-- attribute.
forceAttrMap :: Attr -> AttrMap
forceAttrMap = ForceAttr

-- | Given an attribute and a map, merge the attribute with the map's
-- default attribute. If the map is forcing all lookups to a specific
-- attribute, the forced attribute is returned without merging it with
-- the one specified here. Otherwise the attribute given here is merged
-- with the attribute map's default attribute in that any aspect of the
-- specified attribute that is not provided falls back to the map
-- default. For example,
--
-- @
-- mergeWithDefault (fg blue) $ attrMap (bg red) []
-- @
--
-- returns
--
-- @
-- blue \`on\` red
-- @
mergeWithDefault :: Attr -> AttrMap -> Attr
mergeWithDefault _ (ForceAttr a) = a
mergeWithDefault a (AttrMap d _) = combineAttrs d a

-- | Look up the specified attribute name in the map. Map lookups
-- proceed as follows. If the attribute map is forcing all lookups to a
-- specific attribute, that attribute is returned. If the attribute name
-- is empty, the map's default attribute is returned. If the attribute
-- name is non-empty, very subsequence of names from the specified name
-- are used to perform a lookup, and the results are combined as in
-- 'mergeWithDefault', with more specific results taking precedence over
-- less specific ones.
--
-- For example:
--
-- @
-- attrMapLookup ("foo" <> "bar") (attrMap a []) == a
-- attrMapLookup ("foo" <> "bar") (attrMap (bg blue) [("foo" <> "bar", fg red)]) == red \`on\` blue
-- attrMapLookup ("foo" <> "bar") (attrMap (bg blue) [("foo" <> "bar", red `on` cyan)]) == red \`on\` cyan
-- attrMapLookup ("foo" <> "bar") (attrMap (bg blue) [("foo" <> "bar", fg red), ("foo", bg cyan)]) == red \`on\` cyan
-- attrMapLookup ("foo" <> "bar") (attrMap (bg blue) [("foo", fg red)]) == red \`on\` blue
-- @
attrMapLookup :: AttrName -> AttrMap -> Attr
attrMapLookup _ (ForceAttr a) = a
attrMapLookup (AttrName []) (AttrMap theDefault _) = theDefault
attrMapLookup (AttrName ns) (AttrMap theDefault m) =
    let results = catMaybes $ (\n -> M.lookup n m) <$> (AttrName <$> (inits ns))
    in foldl combineAttrs theDefault results

-- | Set the default attribute value in an attribute map.
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

-- | Insert a set of attribute mappings to an attribute map.
applyAttrMappings :: [(AttrName, Attr)] -> AttrMap -> AttrMap
applyAttrMappings _ (ForceAttr a) = ForceAttr a
applyAttrMappings ms (AttrMap d m) = AttrMap d ((M.fromList ms) `M.union` m)

-- | Update an attribute map such that a lookup of 'ontoName' returns
-- the attribute value specified by 'fromName'.  This is useful for
-- composite widgets with specific attribute names mapping those names
-- to the sub-widget's expected name when calling that sub-widget's
-- rendering function.  See the ProgressBarDemo for an example usage,
-- and 'overrideAttr' for an alternate syntax.
mapAttrName :: AttrName -> AttrName -> AttrMap -> AttrMap
mapAttrName fromName ontoName inMap =
    applyAttrMappings [(ontoName, attrMapLookup fromName inMap)] inMap

-- | Map several attributes to return the value associated with an
-- alternate name.  Applies 'mapAttrName' across a list of mappings.
mapAttrNames :: [(AttrName, AttrName)] -> AttrMap -> AttrMap
mapAttrNames names inMap = foldr (uncurry mapAttrName) inMap names
