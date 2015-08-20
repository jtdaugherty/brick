-- | This module provides an API for turning "markup" values into
-- widgets. This module uses the Data.Text.Markup interface in this
-- package to assign attributes to substrings in a text string; to
-- manipulate markup using (for example) syntax highlighters, see that
-- module.
module Brick.Markup
  ( Markup
  , markup
  , (@?)
  , GetAttr(..)
  )
where

import Control.Lens ((.~), (&), (^.))
import Control.Monad (forM)
import qualified Data.Text as T
import Data.Text.Markup
import Data.Default (def)

import Graphics.Vty (Attr, horizCat, string)

import Brick.Widgets.Core
import Brick.AttrMap
import Brick.Types

-- | A type class for types that provide access to an attribute in the
-- rendering monad.  You probably won't need to instance this.
class GetAttr a where
    -- | Where to get the attribute for this attribute metadata.
    getAttr :: a -> RenderM Attr

instance GetAttr Attr where
    getAttr a = do
        c <- getContext
        return $ mergeWithDefault a (c^.ctxAttrMapL)

instance GetAttr AttrName where
    getAttr = lookupAttrName

-- | Build a piece of markup from text with an assigned attribute name.
-- When the markup is rendered, the attribute name will be looked up in
-- the rendering context's 'AttrMap' to determine the attribute to use
-- for this piece of text.
(@?) :: T.Text -> AttrName -> Markup AttrName
(@?) = (@@)

-- | Build a widget from markup.
markup :: (Eq a, GetAttr a) => Markup a -> Widget
markup m =
    Widget Fixed Fixed $ do
      let pairs = markupToList m
      imgs <- forM pairs $ \(t, aSrc) -> do
          a <- getAttr aSrc
          return $ string a $ T.unpack t
      return $ def & imageL .~ horizCat imgs
