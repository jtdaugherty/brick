{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , str
  )

data Name = VP1
          | VP2
          | VP3
          deriving (Ord, Show, Eq)

drawUi :: () -> [Widget Name]
drawUi = const [ui]
    where
        ui = C.center $ B.border $ hLimit 60 $ vLimit 21 $
             vBox [ pair, B.hBorder, singleton ]
        singleton = viewport VP3 Both $
                    vBox $ str "Press ctrl-arrow keys to scroll this viewport horizontally and vertically."
                         : (str <$> [ "Line " <> show i | i <- [2..25::Int] ])
        pair = hBox [ viewport VP1 Vertical $
                      vBox $ str "Press up and down arrow keys" :
                             str "to scroll this viewport." :
                             (str <$> [ "Line " <> (show i) | i <- [3..50::Int] ])
                    , B.vBorder
                    , viewport VP2 Horizontal $
                      str "Press left and right arrow keys to scroll this viewport."
                    ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

vp2Scroll :: M.ViewportScroll Name
vp2Scroll = M.viewportScroll VP2

vp3Scroll :: M.ViewportScroll Name
vp3Scroll = M.viewportScroll VP3

appEvent :: T.BrickEvent Name e -> T.EventM Name () ()
appEvent (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) = M.vScrollBy vp3Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KUp    [V.MCtrl])) = M.vScrollBy vp3Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = M.hScrollBy vp3Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) = M.hScrollBy vp3Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KRight [])) = M.hScrollBy vp2Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KLeft []))  = M.hScrollBy vp2Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = return ()

app :: M.App () e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr []
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app ()
