{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Data.Monoid ((<>))
import Graphics.Vty
import Data.Text.Markup ((@@))

import Brick.Main
import Brick.Util
import Brick.Render
import Brick.Markup

ui :: Render
ui = markup $ ("Hello" @@ (green `on` blue)) <> ", " <> ("world!" @@ (red `on` black))

main :: IO ()
main = simpleMain [ui]
