{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Applicative
import Control.Monad (void)
import Data.Monoid
import Data.Default
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
  ( Widget
  , ViewportType(Horizontal, Vertical)
  , hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  )

vp1Name :: T.Name
vp1Name = "demo1"

vp2Name :: T.Name
vp2Name = "demo2"

drawUi :: () -> [Widget]
drawUi = const [ui]
    where
        ui = C.center $
             hLimit 60 $
             vLimit 20 $
             B.border $
             hBox [ viewport vp1Name Vertical $
                    vBox $ "Press up and down arrow keys" :
                           "to scroll this viewport." :
                           (str <$> [ "Line " <> (show i) | i <- [3..50::Int] ])
                  , B.vBorder
                  , viewport vp2Name Horizontal
                    "Press left and right arrow keys to scroll this viewport."
                  ]

vp1Scroll :: M.ViewportScroll
vp1Scroll = M.viewportScroll vp1Name

vp2Scroll :: M.ViewportScroll
vp2Scroll = M.viewportScroll vp2Name

appEvent :: () -> V.Event -> M.EventM (M.Next ())
appEvent _ (V.EvKey V.KDown [])  = M.scrollBy vp1Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KUp [])    = M.scrollBy vp1Scroll (-1) >> M.continue ()
appEvent _ (V.EvKey V.KRight []) = M.scrollBy vp2Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KLeft [])  = M.scrollBy vp2Scroll (-1) >> M.continue ()
appEvent _ (V.EvKey V.KEsc []) = M.halt ()
appEvent _ _ = M.continue ()

app :: M.App () V.Event
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const def
          , M.appLiftVtyEvent = id
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app ()
