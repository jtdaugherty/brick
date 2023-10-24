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
import Graphics.Vty.CrossPlatform (mkVty)

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
  , withVScrollBarRenderer
  , withVScrollBarHandles
  , withHScrollBarHandles
  , withClickableHScrollBars
  , withClickableVScrollBars
  , VScrollbarRenderer(..)
  , HScrollbarRenderer(..)
  , scrollbarAttr
  , scrollbarHandleAttr
  )

customHScrollbars :: HScrollbarRenderer n
customHScrollbars =
    HScrollbarRenderer { renderHScrollbar = vLimit 1 $ fill '^'
                       , renderHScrollbarTrough = vLimit 1 $ fill ' '
                       , renderHScrollbarHandleBefore = str "<<"
                       , renderHScrollbarHandleAfter = str ">>"
                       , scrollbarHeightAllocation = 2
                       }

customVScrollbars :: VScrollbarRenderer n
customVScrollbars =
    VScrollbarRenderer { renderVScrollbar = C.hCenter $ hLimit 1 $ fill '*'
                       , renderVScrollbarTrough = fill ' '
                       , renderVScrollbarHandleBefore = C.hCenter $ str "-^-"
                       , renderVScrollbarHandleAfter = C.hCenter $ str "-v-"
                       , scrollbarWidthAllocation = 5
                       }

data Name = VP1 | VP2 | SBClick T.ClickableScrollbarElement Name
          deriving (Ord, Show, Eq)

data St = St { _lastClickedElement :: Maybe (T.ClickableScrollbarElement, Name) }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st = [ui]
    where
        ui = C.center $ hLimit 80 $ vLimit 21 $
             (vBox [ pair
                   , C.hCenter (str "Last clicked scroll bar element:")
                   , str $ show $ _lastClickedElement st
                   ])
        pair = hBox [ padRight (Pad 5) $
                      B.border $
                      withClickableHScrollBars SBClick $
                      withHScrollBars OnBottom $
                      withHScrollBarRenderer customHScrollbars $
                      withHScrollBarHandles $
                      viewport VP1 Horizontal $
                      str $ "Press left and right arrow keys to scroll this viewport.\n" <>
                            "This viewport uses a\n" <>
                            "custom scroll bar renderer!"
                    , B.border $
                      withClickableVScrollBars SBClick $
                      withVScrollBars OnLeft $
                      withVScrollBarRenderer customVScrollbars $
                      withVScrollBarHandles $
                      viewport VP2 Both $
                      vBox $
                      (str $ unlines $
                       [ "Press up and down arrow keys to"
                       , "scroll this viewport vertically."
                       , "This viewport uses a custom"
                       , "scroll bar renderer with"
                       , "a larger space allocation and"
                       , "even more fancy rendering."
                       ])
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
    lastClickedElement .= Just (el, n)
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
          v <- mkVty V.defaultConfig
          V.setMode (V.outputIface v) V.Mouse True
          return v

    initialVty <- buildVty
    void $ M.customMain initialVty buildVty Nothing app (St Nothing)
