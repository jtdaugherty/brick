{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (void)
import Control.Lens
import Data.Monoid
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import qualified Brick.Main as M
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Border as B
import Brick.AttrMap (AttrMap, AttrName, attrMap)
import Brick.Util (on)
import Brick.Types
  ( Widget
  , ViewportType(Horizontal, Vertical, Both)
  )
import Brick.Widgets.Core
  ( withAttr
  , hLimit
  , vLimit
  , hBox
  , vBox
  , viewport
  , multilineStr
  , str
  , visible
  )

data St =
    St { _vp1Index :: Int
       , _vp2Index :: Int
       , _vp3Index :: (Int, Int)
       }

makeLenses ''St

vp1Name :: T.Name
vp1Name = "demo1"

vp1Size :: Int
vp1Size = 15

vp2Name :: T.Name
vp2Name = "demo2"

vp2Size :: Int
vp2Size = 15

vp3Name :: T.Name
vp3Name = "demo3"

vp3Size :: (Int, Int)
vp3Size = (25, 25)

selectedAttr :: AttrName
selectedAttr = "selected"

drawUi :: St -> [Widget]
drawUi st = [ui]
    where
        ui = C.center $ hLimit 60 $ vLimit 30 $
             vBox [ B.border $ vBox [ pair, B.hBorder, singleton ]
                  , multilineStr $ "- Up/down arrow keys scroll the top-left viewport\n" <>
                          "- Left/right arrow keys scroll the top-right viewport\n" <>
                          "- Ctrl-arrow keys move the bottom viewport"
                  ]
        singleton = viewport vp3Name Both $
                    vBox $ do
                         i <- [1..vp3Size^._1]
                         let row = do
                               j <- [1..vp3Size^._2]
                               let mkItem = if (i, j) == st^.vp3Index
                                            then withAttr selectedAttr . visible
                                            else id
                               return $ mkItem $ str $ "Item " <> show (i, j) <> " "
                         return $ hBox row

        pair = hBox [ vp1, B.vBorder, vp2 ]
        vp1 = viewport vp1Name Vertical $
                  vBox $ do
                      i <- [1..vp1Size]
                      let mkItem = if i == st^.vp1Index
                                   then withAttr selectedAttr . visible
                                   else id
                      return $ mkItem $ str $ "Item " <> show i
        vp2 = viewport vp2Name Horizontal $
                  hBox $ do
                      i <- [1..vp2Size]
                      let mkItem = if i == st^.vp2Index
                                   then withAttr selectedAttr . visible
                                   else id
                      return $ mkItem $ str $ "Item " <> show i <> " "

vp1Scroll :: M.ViewportScroll
vp1Scroll = M.viewportScroll vp1Name

vp2Scroll :: M.ViewportScroll
vp2Scroll = M.viewportScroll vp2Name

vp3Scroll :: M.ViewportScroll
vp3Scroll = M.viewportScroll vp3Name

appEvent :: St -> V.Event -> M.EventM (M.Next St)
appEvent st (V.EvKey V.KDown  [V.MCtrl]) = M.continue $ st & vp3Index._1 %~ min (vp3Size^._1) . (+ 1)
appEvent st (V.EvKey V.KUp    [V.MCtrl]) = M.continue $ st & vp3Index._1 %~ max 1 . subtract 1
appEvent st (V.EvKey V.KRight [V.MCtrl]) = M.continue $ st & vp3Index._2 %~ min (vp3Size^._1) . (+ 1)
appEvent st (V.EvKey V.KLeft  [V.MCtrl]) = M.continue $ st & vp3Index._2 %~ max 1 .  subtract 1
appEvent st (V.EvKey V.KDown [])         = M.continue $ st & vp1Index %~ min vp1Size . (+ 1)
appEvent st (V.EvKey V.KUp [])           = M.continue $ st & vp1Index %~ max 1 . subtract 1
appEvent st (V.EvKey V.KRight [])        = M.continue $ st & vp2Index %~ min vp2Size . (+ 1)
appEvent st (V.EvKey V.KLeft [])         = M.continue $ st & vp2Index %~ max 1 . subtract 1
appEvent st (V.EvKey V.KEsc []) = M.halt st
appEvent st _ = M.continue st

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (selectedAttr, V.black `on` V.yellow)
    ]

app :: M.App St V.Event
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          , M.appChooseCursor = M.neverShowCursor
          }

initialState :: St
initialState = St 1 1 (1, 1)

main :: IO ()
main = void $ M.defaultMain app initialState
