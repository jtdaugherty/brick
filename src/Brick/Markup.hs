module Brick.Markup
  ( markup
  )
where

import Control.Applicative ((<$>))
import Control.Lens ((.~), (&))
import qualified Data.Text as T
import Data.Text.Markup
import Data.Default (def)

import Graphics.Vty (Attr, horizCat, string)

import Brick.Render (Render, image)

markup :: Markup Attr -> Render
markup m =
    let pairs = toList m
        imgs = mkImage <$> pairs
        mkImage (t, a) = string a $ T.unpack t
    in return $ def & image .~ horizCat imgs
