{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid ((<>))
import Graphics.Vty
import Data.Text.Markup ((@@))

import Brick.Main
import Brick.Util
import Brick.Render
import Brick.Markup
import Brick.AttrMap

ui :: Render
ui = m1 <=> m2
    where
        -- Two ways to assign attributes to text in markup: via
        -- attributes (direct) or via attribute names (indirect)
        m1 = markup $ ("Hello" @? "kw1") <> ", " <> ("world!" @? "kw2")
        m2 = markup $ ("Hello" @@ fg red) <> ", " <> ("world!" @@ (yellow `on` black))

aMap :: [(AttrName, Attr)]
aMap =
    [ ("kw1", fg green)
    , ("kw2", red `on` black)
    ]

main :: IO ()
main = simpleMain aMap [ui]
