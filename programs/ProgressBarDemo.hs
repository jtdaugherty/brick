{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad (void)
import Data.Monoid
import qualified Graphics.Vty as V

import qualified Brick.AttrMap as A
import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.ProgressBar as P
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<+>), (<=>)
  , str
  , updateAttrMap
  , overrideAttr
  )
import Brick.Util (fg, bg, on, clamp)

data MyAppState n = MyAppState { x, y, z :: Float }

drawUI :: MyAppState () -> [Widget ()]
drawUI p = [ui]
    where
      -- use mapAttrNames
      xBar = updateAttrMap
             (A.mapAttrNames [ (xDoneAttr, P.progressCompleteAttr)
                             , (xToDoAttr, P.progressIncompleteAttr)
                             ]
             ) $ bar $ x p
      -- or use individual mapAttrName calls
      yBar = updateAttrMap
             (A.mapAttrName yDoneAttr P.progressCompleteAttr .
              A.mapAttrName yToDoAttr P.progressIncompleteAttr) $
             bar $ y p
      -- or use overrideAttr calls
      zBar = overrideAttr P.progressCompleteAttr zDoneAttr $
             overrideAttr P.progressIncompleteAttr zToDoAttr $
             bar $ z p
      lbl c = Just $ show $ fromEnum $ c * 100
      bar v = P.progressBar (lbl v) v
      ui = (str "X: " <+> xBar) <=>
           (str "Y: " <+> yBar) <=>
           (str "Z: " <+> zBar) <=>
           str "" <=>
           str "Hit 'x', 'y', or 'z' to advance progress, or 'q' to quit"

appEvent :: MyAppState () -> V.Event -> T.EventM () (T.Next (MyAppState ()))
appEvent p e =
    let valid = clamp (0.0 :: Float) 1.0
    in case e of
         V.EvKey (V.KChar 'x') [] -> M.continue $ p { x = valid $ x p + 0.05 }
         V.EvKey (V.KChar 'y') [] -> M.continue $ p { y = valid $ y p + 0.03 }
         V.EvKey (V.KChar 'z') [] -> M.continue $ p { z = valid $ z p + 0.02 }
         V.EvKey (V.KChar 'q') [] -> M.halt p
         _ -> M.continue p

initialState :: MyAppState ()
initialState = MyAppState 0.25 0.18 0.63

theBaseAttr :: A.AttrName
theBaseAttr = A.attrName "theBase"

xDoneAttr, xToDoAttr :: A.AttrName
xDoneAttr = theBaseAttr <> A.attrName "X:done"
xToDoAttr = theBaseAttr <> A.attrName "X:remaining"

yDoneAttr, yToDoAttr :: A.AttrName
yDoneAttr = theBaseAttr <> A.attrName "Y:done"
yToDoAttr = theBaseAttr <> A.attrName "Y:remaining"

zDoneAttr, zToDoAttr :: A.AttrName
zDoneAttr = theBaseAttr <> A.attrName "Z:done"
zToDoAttr = theBaseAttr <> A.attrName "Z:remaining"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
         [ (theBaseAttr,               bg V.brightBlack)
         , (xDoneAttr,                 V.black `on` V.white)
         , (xToDoAttr,                 V.white `on` V.black)
         , (yDoneAttr,                 V.magenta `on` V.yellow)
         , (zDoneAttr,                 V.blue `on` V.green)
         , (zToDoAttr,                 V.blue `on` V.red)
         , (P.progressIncompleteAttr,  fg V.yellow)
         ]

theApp :: M.App (MyAppState ()) V.Event ()
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
