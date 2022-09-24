{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro.TH
import Lens.Micro.Mtl
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
  ( Padding(..)
  , hLimit
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
  , withVScrollBarHandles
  , withHScrollBarHandles
  , withClickableHScrollBars
  , withClickableVScrollBars
  , ScrollbarRenderer(..)
  , scrollbarAttr
  , scrollbarHandleAttr
  )

customScrollbars :: ScrollbarRenderer n
customScrollbars =
    ScrollbarRenderer { renderScrollbar = fill '^'
                      , renderScrollbarTrough = fill ' '
                      , renderScrollbarHandleBefore = str "<<"
                      , renderScrollbarHandleAfter = str ">>"
                      }

data Name = VP1 | VP2 | SBClick T.ClickableScrollbarElement Name
          deriving (Ord, Show, Eq)

data St = St { _lastClickedElement :: Maybe (T.ClickableScrollbarElement, Name) }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = [ui]
    where
        ui = C.center $ hLimit 70 $ vLimit 21 $
             (vBox [ pair
                   , C.hCenter (str "Last clicked scroll bar element:")
                   , str $ show $ _lastClickedElement st
                   ])
        pair = hBox [ padRight (Pad 5) $
                      B.border $
                      withClickableHScrollBars SBClick $
                      withHScrollBars OnBottom $
                      withHScrollBarRenderer customScrollbars $
                      withHScrollBarHandles $
                      viewport VP1 Horizontal $
                      str $ "Press left and right arrow keys to scroll this viewport.\n" <>
                            "This viewport uses a\n" <>
                            "custom scroll bar renderer!"
                    , B.border $
                      withClickableVScrollBars SBClick $
                      withVScrollBars OnLeft $
                      withVScrollBarHandles $
                      viewport VP2 Both $
                      vBox $ str "Press ctrl-arrow keys to scroll this viewport horizontally and vertically."
                      : (str <$> [ "Line " <> show i | i <- [2..55::Int] ])
                    ]

vp1Scroll :: M.ViewportScroll Name
vp1Scroll = M.viewportScroll VP1

vp2Scroll :: M.ViewportScroll Name
vp2Scroll = M.viewportScroll VP2

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KRight []))  = M.hScrollBy vp1Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KLeft []))   = M.hScrollBy vp1Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KDown []))   = M.vScrollBy vp2Scroll 1
appEvent (T.VtyEvent (V.EvKey V.KUp []))     = M.vScrollBy vp2Scroll (-1)
appEvent (T.VtyEvent (V.EvKey V.KEsc []))    = M.halt
appEvent (T.MouseDown (SBClick el n) _ _ _) = do
    case n of
        VP1 -> do
            let vp = M.viewportScroll VP1
            case el of
                T.SBHandleBefore -> M.hScrollBy vp (-1)
                T.SBHandleAfter  -> M.hScrollBy vp 1
                T.SBTroughBefore -> M.hScrollBy vp (-10)
                T.SBTroughAfter  -> M.hScrollBy vp 10
                T.SBBar          -> return ()
        VP2 -> do
            let vp = M.viewportScroll VP2
            case el of
                T.SBHandleBefore -> M.vScrollBy vp (-1)
                T.SBHandleAfter  -> M.vScrollBy vp 1
                T.SBTroughBefore -> M.vScrollBy vp (-10)
                T.SBTroughAfter  -> M.vScrollBy vp 10
                T.SBBar          -> return ()
        _ ->
            return ()

    lastClickedElement .= Just (el, n)
appEvent _ = return ()

theme :: AttrMap
theme =
    attrMap V.defAttr
    [ (scrollbarAttr,       fg V.white)
    , (scrollbarHandleAttr, fg V.brightYellow)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theme
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = do
    let buildVty = do
          v <- V.mkVty =<< V.standardIOConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ M.customMain initialVty buildVty Nothing app (St Nothing)
