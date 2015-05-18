{-# LANGUAGE OverloadedStrings #-}
module Brick.Border
  ( bordered
  , borderedWithLabel
  )
where

import Brick.Prim
import Brick.Centering (hCenteredWith)

bordered :: Prim -> Prim
bordered = bordered_ Nothing

borderedWithLabel :: String -> Prim -> Prim
borderedWithLabel label = bordered_ (Just label)

bordered_ :: Maybe String -> Prim -> Prim
bordered_ label wrapped = total
    where
        labelStr = maybe (Txt "") Txt label
        top = "+" <<+ hCenteredWith '-' labelStr +>> "+"
        bottom = "+" <<+ HFill '-' +>> "+"
        middle = VFill '|' +>> wrapped <<+ VFill '|'
        total = top =>> middle <<= bottom
