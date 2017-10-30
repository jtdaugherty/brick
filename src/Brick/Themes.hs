{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Themes
  ( CustomAttr(..)
  , customFgL
  , customBgL
  , customStyleL

  , Theme(..)
  , themeDefaultAttrL
  , themeDefaultMappingL
  , themeCustomMappingL
  , themeCustomDefaultAttrL

  , themeToAttrMap
  )
where

import GHC.Generics (Generic)
import Graphics.Vty
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F

import Brick.AttrMap (AttrMap, AttrName, attrMap)
import Brick.Types.TH (suffixLenses)

-- | An attribute customization can specify which aspects of an
-- attribute to customize.
data CustomAttr =
    CustomAttr { customFg    :: Maybe (MaybeDefault Color)
               -- ^ The customized foreground, if any.
               , customBg    :: Maybe (MaybeDefault Color)
               -- ^ The customized background, if any.
               , customStyle :: Maybe Style
               -- ^ The customized style, if any.
               }
               deriving (Eq, Read, Show, Generic)

-- | A theme provides a set of default attribute mappings, a default
-- attribute, and a set of customizations for the default mapping
-- and default attribute. The idea here is that the application will
-- always need to provide a complete specification of its attribute
-- mapping, but if the user wants to customize any aspect of that
-- default mapping, it can be contained here and then built into an
-- 'AttrMap' (see 'themeToAttrMap'). We keep the defaults separate
-- from customizations to permit users to serialize themes and their
-- customizations to, say, disk files.
data Theme =
    Theme { themeDefaultAttr :: Attr
          -- ^ The default attribute to use.
          , themeCustomDefaultAttr :: CustomAttr
          -- ^ Customization for the theme's default attribute.
          , themeDefaultMapping :: M.Map AttrName (Attr, T.Text)
          -- ^ The default attribute mapping to use. This maps attribute
          -- names to default attributes, but it also requires the
          -- author to provide human-readable documentation strings for
          -- each attribute describing what the attribute affects in the
          -- interface. This is to aid the generation of documentation
          -- for user customization of the theme.
          , themeCustomMapping :: M.Map AttrName CustomAttr
          -- ^ Customizations for individual entries of the default
          -- mapping. Note that this will only affect entries in the
          -- default mapping; any attributes named here that are not
          -- present in the default mapping will not be considered.
          }
          deriving (Eq, Read, Show, Generic)

suffixLenses ''CustomAttr
suffixLenses ''Theme

-- | Build an 'AttrMap' from a 'Theme'. This applies all customizations
-- in the returned 'AttrMap'.
themeToAttrMap :: Theme -> AttrMap
themeToAttrMap t =
    attrMap (customizeAttr (themeCustomDefaultAttr t) (themeDefaultAttr t)) customMap
    where
        customMap = F.foldr f [] (M.toList $ themeDefaultMapping t)
        f (aName, (attr, _)) mapping =
            let a' = case M.lookup aName (themeCustomMapping t) of
                       Nothing     -> attr
                       Just custom -> customizeAttr custom attr
            in (aName, a'):mapping

customizeAttr :: CustomAttr -> Attr -> Attr
customizeAttr c a =
    let fg = fromMaybe (attrForeColor a) (customFg c)
        bg = fromMaybe (attrBackColor a) (customBg c)
        sty = maybe (attrStyle a) SetTo (customStyle c)
    in a { attrForeColor = fg
         , attrBackColor = bg
         , attrStyle = sty
         }
