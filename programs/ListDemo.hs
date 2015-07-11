{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens ((^.))
import Control.Monad (void)
import Data.Monoid
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Widgets.Core
  ( Widget
  , (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

drawUI :: L.List Int -> [Widget]
drawUI l = [ui]
    where
        label = "Item " <+> cur <+> " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i -> str (show (i + 1))
        total = str $ show $ length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              L.renderList l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , " "
                              , C.hCenter "Press +/- to add/remove list elements."
                              , C.hCenter "Press Esc to exit."
                              ]

appEvent :: L.List Int -> V.Event -> M.EventM (M.Next (L.List Int))
appEvent l e =
    case e of
        V.EvKey (V.KChar '+') [] ->
            let el = length $ l^.(L.listElementsL)
            in M.continue $ L.listInsert el el l

        V.EvKey (V.KChar '-') [] ->
            case l^.(L.listSelectedL) of
                Nothing -> M.continue l
                Just i -> M.continue $ L.listRemove i l

        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue $ T.handleEvent ev l

listDrawElement :: Bool -> Int -> Widget
listDrawElement sel i =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ "Item " <+> (selStr $ show i)

initialState :: L.List Int
initialState = L.list (T.Name "list") listDrawElement [0, 1, 2]

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List Int) V.Event
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
