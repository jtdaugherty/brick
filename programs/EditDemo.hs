{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Lens
import qualified Data.Vector as DV
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , fill
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import qualified Brick.Focus as F
import Brick.Util (on)

data Name = Edit1
          | Edit2
          | List1
          deriving (Ord, Show, Eq)

data St =
    St { _focusRing :: F.FocusRing Name
       , _edit1 :: E.Editor Name
       , _edit2 :: E.Editor Name
       , _list1 :: L.List Name Int
       }

makeLenses ''St

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        theList = F.withFocusRing (st^.focusRing) (L.renderList drawElem) (st^.list1)
        e1 = F.withFocusRing (st^.focusRing) E.renderEditor (st^.edit1)
        e2 = F.withFocusRing (st^.focusRing) E.renderEditor (st^.edit2)

        drawElem _ i = (str $ show i) <+> (vLimit 1 $ fill ' ')
        ui = C.center $
            (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 e1)) <=>
            str " " <=>
            (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 e2)) <=>
            str " " <=>
            (str "Input 3: " <+> (hLimit 30 $ vLimit 3 theList)) <=>
            str " " <=>
            str "Press Tab to switch between editors, Esc to quit."

appEvent :: St -> V.Event -> T.EventM Name (T.Next St)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ st & focusRing %~ F.focusNext
        V.EvKey V.KBackTab [] -> M.continue $ st & focusRing %~ F.focusPrev

        _ -> M.continue =<< case F.focusGetCurrent (st^.focusRing) of
               Just Edit1 -> T.handleEventLensed st edit1 E.handleEditorEvent ev
               Just Edit2 -> T.handleEventLensed st edit2 E.handleEditorEvent ev
               Just List1 -> T.handleEventLensed st list1 L.handleListEvent ev
               Nothing -> return st

initialState :: St
initialState =
    St (F.focusRing [Edit1, Edit2, List1])
       (E.editor Edit1 (str . unlines) Nothing "")
       (E.editor Edit2 (str . unlines) (Just 2) "")
       (L.list List1 (DV.fromList [1, 2, 3, 4, 5]) 1)

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr,                   V.white `on` V.blue)
    , (L.listSelectedAttr,           V.white `on` V.blue)

    , (E.editFocusedAttr,            V.black `on` V.yellow)
    , (L.listSelectedFocusedAttr,    V.black `on` V.yellow)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor = F.focusRingCursor (^.focusRing)

theApp :: M.App St V.Event Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = appCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          }

main :: IO ()
main = do
    st <- M.defaultMain theApp initialState
    putStrLn "In input 1 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit1
    putStrLn "In input 2 you entered:\n"
    putStrLn $ unlines $ E.getEditContents $ st^.edit2
