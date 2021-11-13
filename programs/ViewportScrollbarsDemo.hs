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
  , ViewportType(Horizontal, Both)
  , VScrollBarOrientation(..)
  , HScrollBarOrientation(..)
  )
import Brick.Util
  ( fg
  )
import Brick.AttrMap
  ( AttrMap
  , attrMap
  )
import Brick.Widgets.Core
  ( hLimit
  , vLimit
  , padRight
  , hBox
  , vBox
  , viewport
  , str
  , fill
  , withVScrollBars
  , withHScrollBars
  , withHScrollBarRenderer
  , ScrollbarRenderer(..)
  , scrollbarAttr
  , scrollbarTroughAttr
  )

customSB :: ScrollbarRenderer n
customSB =
    ScrollbarRenderer { renderScrollbar = fill '^'
                      , renderScrollbarTrough = fill '_'
                      }

data Name = VP1
          | VP2
          | VP3
          deriving (Ord, Show, Eq)

drawUi :: () -> [Widget Name]
drawUi = const [ui]
    where
        ui = C.center $ hLimit 70 $ vLimit 21 pair
        pair = hBox [ padRight (T.Pad 5) $
                      B.border $
                      withHScrollBars OnBottom $
                      withHScrollBarRenderer customSB $
                      viewport VP2 Horizontal $
                      str $ "Press left and right arrow keys to scroll this viewport.\n" <>
                            "This viewport uses a\n" <>
                            "custom scroll bar renderer!"
                    , B.border $
                      withVScrollBars OnLeft $
                      viewport VP3 Both $
                      vBox $ str "Press ctrl-arrow keys to scroll this viewport horizontally and vertically."
                      : (str <$> [ "Line " <> show i | i <- [2..55::Int] ])
                    ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

vp2Scroll :: M.ViewportScroll Name
vp2Scroll = M.viewportScroll VP2

vp3Scroll :: M.ViewportScroll Name
vp3Scroll = M.viewportScroll VP3

appEvent :: () -> T.BrickEvent Name e -> T.EventM Name (T.Next ())
appEvent _ (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) = M.vScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KUp    [V.MCtrl])) = M.vScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = M.hScrollBy vp3Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) = M.hScrollBy vp3Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KDown []))  = M.vScrollBy vp1Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KUp []))    = M.vScrollBy vp1Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KRight [])) = M.hScrollBy vp2Scroll 1 >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KLeft []))  = M.hScrollBy vp2Scroll (-1) >> M.continue ()
appEvent _ (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt ()
appEvent _ _ = M.continue ()

theme :: AttrMap
theme =
    attrMap V.defAttr $
    [ (scrollbarAttr,       fg V.white)
    , (scrollbarTroughAttr, fg V.red)
    ]

app :: M.App () e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theme
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app ()
