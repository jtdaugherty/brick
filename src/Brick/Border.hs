{-# LANGUAGE OverloadedStrings #-}
module Brick.Border
  ( bordered
  , borderedWithLabel
  )
where

import Brick.Prim
import Brick.Center (hCenteredWith)

bordered :: Prim a -> Prim a
bordered = bordered_ Nothing

borderedWithLabel :: String -> Prim a -> Prim a
borderedWithLabel label = bordered_ (Just label)

bordered_ :: Maybe String -> Prim a -> Prim a
bordered_ label wrapped = total
    where
        labelStr = maybe (Txt "") Txt label
        top = "+" <<+ hCenteredWith '-' labelStr +>> "+"
        bottom = "+" <<+ HFill '-' +>> "+"
        middle = VFill '|' +>> wrapped <<+ VFill '|'
        total = top =>> middle <<= bottom
