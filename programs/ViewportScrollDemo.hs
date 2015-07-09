{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad (void)
import Data.Monoid
import Data.Default
import Graphics.Vty

import Brick.Types
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

vp1Name :: Name
vp1Name = "demo1"

vp2Name :: Name
vp2Name = "demo2"

drawUi :: () -> [Widget]
drawUi = const [ui]
    where
        ui = center $
             hLimit 60 $
             vLimit 20 $
             border $
             hBox [ viewport vp1Name Vertical $
                    vBox $ "Press up and down arrow keys" :
                           "to scroll this viewport." :
                           (str <$> [ "Line " <> (show i) | i <- [3..50::Int] ])
                  , vBorder
                  , viewport vp2Name Horizontal
                    "Press left and right arrow keys to scroll this viewport."
                  ]

vp1Scroll :: ViewportScroll
vp1Scroll = viewportScroll vp1Name

vp2Scroll :: ViewportScroll
vp2Scroll = viewportScroll vp2Name

appEvent :: () -> Event -> EventM (Next ())
appEvent _ (EvKey KDown [])  = scrollBy vp1Scroll 1 >> continue ()
appEvent _ (EvKey KUp [])    = scrollBy vp1Scroll (-1) >> continue ()
appEvent _ (EvKey KRight []) = scrollBy vp2Scroll 1 >> continue ()
appEvent _ (EvKey KLeft [])  = scrollBy vp2Scroll (-1) >> continue ()
appEvent _ (EvKey KEsc []) = halt ()
appEvent _ _ = continue ()

app :: App () Event
app =
    App { appDraw = drawUi
        , appStartEvent = return
        , appHandleEvent = appEvent
        , appAttrMap = const def
        , appMakeVtyEvent = id
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = void $ defaultMain app ()
