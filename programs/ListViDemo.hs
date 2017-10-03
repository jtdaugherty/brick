{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Monoid
import qualified Graphics.Vty as V
import Lens.Micro ((^.))

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import Brick.Types (Widget)
import qualified Brick.Types as T
import Brick.Util (fg, on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core (hLimit, str, vBox, vLimit, withAttr, (<+>))
import qualified Brick.Widgets.List as L
import qualified Data.Vector as Vec

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i  -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              L.renderList listDrawElement True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: L.List () Char -> T.BrickEvent () e -> T.EventM () (T.Next (L.List () Char))
appEvent l (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] ->
            let el = nextElement (L.listElements l)
                pos = Vec.length $ l^.(L.listElementsL)
            in M.continue $ L.listInsert pos el l

        V.EvKey (V.KChar '-') [] ->
            case l^.(L.listSelectedL) of
                Nothing -> M.continue l
                Just i  -> M.continue $ L.listRemove i l

        V.EvKey V.KEsc [] -> M.halt l

        ev -> M.continue =<< (L.handleListEventVi L.handleListEvent) ev l
    where
      nextElement :: Vec.Vector Char -> Char
      nextElement v = fromMaybe '?' $ Vec.find (flip Vec.notElem v) (Vec.fromList ['a' .. 'z'])
appEvent l _ = M.continue l

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

initialState :: L.List () Char
initialState = L.list () (Vec.fromList ['a','b','c']) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List () Char) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
