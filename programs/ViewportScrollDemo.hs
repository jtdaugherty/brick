{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.Widgets.Core
  ( hLimit
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

vp3Name :: T.Name
vp3Name = "demo3"

drawUi :: () -> [Widget]
drawUi = const [ui]
    where
        ui = C.center $ B.border $ hLimit 60 $ vLimit 21 $
             vBox [ pair, B.hBorder, singleton ]
        singleton = viewport vp3Name Both $
                    vBox $ str "Press ctrl-arrow keys to scroll this viewport horizontally and vertically."
                         : (str <$> [ "Line " <> show i | i <- [2..25::Int] ])
        pair = hBox [ viewport vp1Name Vertical $
                      vBox $ str "Press up and down arrow keys" :
                             str "to scroll this viewport." :
                             (str <$> [ "Line " <> (show i) | i <- [3..50::Int] ])
                    , B.vBorder
                    , viewport vp2Name Horizontal $
                      str "Press left and right arrow keys to scroll this viewport."
                    ]

vp1Scroll :: M.ViewportScroll
vp1Scroll = M.viewportScroll vp1Name

vp2Scroll :: M.ViewportScroll
vp2Scroll = M.viewportScroll vp2Name

vp3Scroll :: M.ViewportScroll
vp3Scroll = M.viewportScroll vp3Name

appEvent :: () -> V.Event -> T.EventM (T.Next ())
appEvent _ (V.EvKey V.KDown  [V.MCtrl]) = M.vScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KUp    [V.MCtrl]) = M.vScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (V.EvKey V.KRight [V.MCtrl]) = M.hScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KLeft  [V.MCtrl]) = M.hScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (V.EvKey V.KDown [])  = M.vScrollBy vp1Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KUp [])    = M.vScrollBy vp1Scroll (-1) >> M.continue ()
appEvent _ (V.EvKey V.KRight []) = M.hScrollBy vp2Scroll 1 >> M.continue ()
appEvent _ (V.EvKey V.KLeft [])  = M.hScrollBy vp2Scroll (-1) >> M.continue ()
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
