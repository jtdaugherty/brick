{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.State (modify)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>)
  , str
  , vLimit
  , hLimit
  , vBox
  , withAttr
  )
import Brick.Util (fg, on)

drawUI :: (Show a) => L.List () a -> [Widget ()]
drawUI l = [ui]
    where
        label = str "Item " <+> cur <+> str " of " <+> total
        cur = case l^.(L.listSelectedL) of
                Nothing -> str "-"
                Just i -> str (show (i + 1))
        total = str $ show $ Vec.length $ l^.(L.listElementsL)
        box = B.borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              L.renderList listDrawElement True l
        wrapStatus = if L.getScrollWrap l
                     then "enabled"
                     else "disabled"
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Press Esc to exit."
                              , str " "
                              , C.hCenter $ str "Press 'w' to toggle selection wrapping."
                              , C.hCenter $ str $ "Selection wrapping is currently " <> wrapStatus <> "."
                              ]

appEvent :: T.BrickEvent () e -> T.EventM () (L.List () Int) ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] -> do
            els <- use L.listElementsL
            let pos = Vec.length els
            modify $ L.listInsert pos pos

        V.EvKey (V.KChar '-') [] -> do
            sel <- use L.listSelectedL
            case sel of
                Nothing -> return ()
                Just i -> modify $ L.listRemove i

        V.EvKey (V.KChar 'w') [] ->
            toggleListWrapping

        V.EvKey V.KEsc [] -> M.halt

        ev -> L.handleListEvent ev
appEvent _ = return ()

toggleListWrapping :: T.EventM () (L.List () Int) ()
toggleListWrapping = L.listScrollWrapL %= not

listDrawElement :: (Show a) => Bool -> a -> Widget ()
listDrawElement sel a =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in C.hCenter $ str "Item " <+> (selStr $ show a)

initialState :: L.List () Int
initialState = L.list () (Vec.fromList [0..2000]) 1

customAttr :: A.AttrName
customAttr = L.listSelectedAttr <> A.attrName "custom"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (L.listSelectedAttr,    V.blue `on` V.white)
    , (customAttr,            fg V.cyan)
    ]

theApp :: M.App (L.List () Int) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
