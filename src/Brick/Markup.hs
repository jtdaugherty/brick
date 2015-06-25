module Brick.Markup
  ( Markup
  , markup
  , (@?)
  )
where

import Control.Lens ((.~), (&), (^.))
import Control.Monad (forM)
import qualified Data.Text as T
import Data.Text.Markup
import Data.Default (def)

import Graphics.Vty (Attr, horizCat, string)

import Brick.Render
import Brick.AttrMap

class GetAttr a where
    getAttr :: a -> RenderM Attr

instance GetAttr Attr where
    getAttr a = do
        c <- getContext
        return $ mergeWithDefault a (c^.ctxAttrs)

instance GetAttr AttrName where
    getAttr = lookupAttrName

(@?) :: T.Text -> AttrName -> Markup AttrName
(@?) = (@@)

markup :: (Eq a, GetAttr a) => Markup a -> Widget
markup m =
    Widget $ do
      let pairs = markupToList m
      imgs <- forM pairs $ \(t, aSrc) -> do
          a <- getAttr aSrc
          return $ string a $ T.unpack t
      return $ def & image .~ horizCat imgs
