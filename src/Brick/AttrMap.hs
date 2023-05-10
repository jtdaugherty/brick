{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- | This module provides types and functions for managing an attribute
-- map which maps attribute names ('AttrName') to attributes ('Attr').
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
-- specific name ('parent <> child' as above) is successively merged with
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
  , forceAttrMapAllowStyle
  , attrName
  -- * Inspection
  , attrNameComponents
  -- * Finding attributes from names
  , attrMapLookup
  -- * Manipulating attribute maps
  , setDefaultAttr
  , getDefaultAttr
  , applyAttrMappings
  , mergeWithDefault
  , mapAttrName
  , mapAttrNames
  )
where

import qualified Data.Semigroup as Sem

import Control.DeepSeq
import Data.Bits ((.|.))
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (inits)
import GHC.Generics (Generic)

import Graphics.Vty (Attr(..), MaybeDefault(..), Style)

-- | An attribute name. Attribute names are hierarchical; use 'mappend'
-- ('<>') to assemble them. Hierarchy in an attribute name is used to
-- represent increasing levels of specificity in referring to the
-- attribute you want to use for a visual element, with names to the
-- left being general and names to the right being more specific. For
-- example:
--
-- @
-- attrName "window" <> attrName "border"
-- attrName "window" <> attrName "title"
-- attrName "header" <> attrName "clock" <> attrName "seconds"
-- @
data AttrName = AttrName [String]
              deriving (Show, Read, Eq, Ord, Generic, NFData)

instance Sem.Semigroup AttrName where
    (AttrName as) <> (AttrName bs) = AttrName $ as `mappend` bs

instance Monoid AttrName where
    mempty = AttrName []
    mappend = (Sem.<>)

-- | An attribute map which maps 'AttrName' values to 'Attr' values.
data AttrMap = AttrMap Attr (M.Map AttrName Attr)
             | ForceAttr Attr
             | ForceAttrAllowStyle Attr AttrMap
             deriving (Show, Generic, NFData)

-- | Create an attribute name from a string.
attrName :: String -> AttrName
attrName = AttrName . (:[])

-- | Get the components of an attribute name.
attrNameComponents :: AttrName -> [String]
attrNameComponents (AttrName cs) = cs

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
-- attribute. This is functionally equivalent to @attrMap attr []@.
forceAttrMap :: Attr -> AttrMap
forceAttrMap = ForceAttr

-- | Create an attribute map in which all lookups map to the same
-- attribute. This is functionally equivalent to @attrMap attr []@.
forceAttrMapAllowStyle :: Attr -> AttrMap -> AttrMap
forceAttrMapAllowStyle = ForceAttrAllowStyle

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
mergeWithDefault _ (ForceAttrAllowStyle f _) = f
mergeWithDefault a (AttrMap d _) = combineAttrs d a

-- | Look up the specified attribute name in the map. Map lookups
-- proceed as follows. If the attribute map is forcing all lookups to a
-- specific attribute, that attribute is returned along with its style
-- settings. If the attribute name is empty, the map's default attribute
-- is returned. If the attribute name is non-empty, every subsequence of
-- names from the specified name are used to perform a lookup and the
-- results are combined as in 'mergeWithDefault', with more specific
-- results taking precedence over less specific ones. As attributes are
-- merged, styles are also merged. If a more specific attribute name
-- introduces a style (underline, say) and a less specific attribute
-- name introduces an additional style (bold, say) then the final result
-- will include both styles.
--
-- For example:
--
-- @
-- attrMapLookup (attrName "foo" <> attrName "bar") (attrMap a []) == a
-- attrMapLookup (attrName "foo" <> attrName "bar") (attrMap (bg blue) [(attrName "foo" <> attrName "bar", fg red)]) == red \`on\` blue
-- attrMapLookup (attrName "foo" <> attrName "bar") (attrMap (bg blue) [(attrName "foo" <> attrName "bar", red `on` cyan)]) == red \`on\` cyan
-- attrMapLookup (attrName "foo" <> attrName "bar") (attrMap (bg blue) [(attrName "foo" <> attrName "bar", fg red), ("foo", bg cyan)]) == red \`on\` cyan
-- attrMapLookup (attrName "foo" <> attrName "bar") (attrMap (bg blue) [(attrName "foo", fg red)]) == red \`on\` blue
-- @
attrMapLookup :: AttrName -> AttrMap -> Attr
attrMapLookup _ (ForceAttr a) = a
attrMapLookup a (ForceAttrAllowStyle forced m) =
    -- Look up the attribute in the contained map, then keep only its
    -- style.
    let result = attrMapLookup a m
    in forced { attrStyle = attrStyle forced `combineStyles` attrStyle result
              }
attrMapLookup (AttrName []) (AttrMap theDefault _) = theDefault
attrMapLookup (AttrName ns) (AttrMap theDefault m) =
    let results = mapMaybe (\n -> M.lookup (AttrName n) m) (inits ns)
    in foldl combineAttrs theDefault results

-- | Set the default attribute value in an attribute map.
setDefaultAttr :: Attr -> AttrMap -> AttrMap
setDefaultAttr _ (ForceAttr a) = ForceAttr a
setDefaultAttr newDefault (ForceAttrAllowStyle a m) =
    ForceAttrAllowStyle a (setDefaultAttr newDefault m)
setDefaultAttr newDefault (AttrMap _ m) = AttrMap newDefault m

-- | Get the default attribute value in an attribute map.
getDefaultAttr :: AttrMap -> Attr
getDefaultAttr (ForceAttr a) = a
getDefaultAttr (ForceAttrAllowStyle _ m) = getDefaultAttr m
getDefaultAttr (AttrMap d _) = d

combineAttrs :: Attr -> Attr -> Attr
combineAttrs (Attr s1 f1 b1 u1) (Attr s2 f2 b2 u2) =
    Attr (s1 `combineStyles` s2)
         (f1 `combineMDs` f2)
         (b1 `combineMDs` b2)
         (u1 `combineMDs` u2)

combineMDs :: MaybeDefault a -> MaybeDefault a -> MaybeDefault a
combineMDs _ (SetTo v) = SetTo v
combineMDs (SetTo v) _ = SetTo v
combineMDs _ v = v

combineStyles :: MaybeDefault Style -> MaybeDefault Style -> MaybeDefault Style
combineStyles (SetTo a) (SetTo b) = SetTo $ a .|. b
combineStyles _ (SetTo v) = SetTo v
combineStyles (SetTo v) _ = SetTo v
combineStyles _ v = v

-- | Insert a set of attribute mappings to an attribute map.
applyAttrMappings :: [(AttrName, Attr)] -> AttrMap -> AttrMap
applyAttrMappings _ (ForceAttr a) = ForceAttr a
applyAttrMappings ms (AttrMap d m) = AttrMap d ((M.fromList ms) `M.union` m)
applyAttrMappings ms (ForceAttrAllowStyle a m) = ForceAttrAllowStyle a (applyAttrMappings ms m)

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
