{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Vty (green, red)

import Brick.Main
import Brick.Util
import Brick.Render

ui :: Render
ui =
    ("Hello" @@ (fg green))
    <+> ", "
    <+> ("world!" @@ (fg red))

main :: IO ()
main = simpleMain [ui]
