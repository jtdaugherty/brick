{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Lens
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( (<+>)
  , (<=>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Brick.Util (on)

data Name = Edit1
          | Edit2
          deriving (Ord, Show, Eq)

data St =
    St { _currentEditor :: Name
       , _edit1 :: E.Editor Name
       , _edit2 :: E.Editor Name
       }

makeLenses ''St

switchEditors :: St -> St
switchEditors st =
    let next = if st^.currentEditor == Edit1
               then Edit2 else Edit1
    in st & currentEditor .~ next

currentEditorL :: St -> Lens' St (E.Editor Name)
currentEditorL st =
    if st^.currentEditor == Edit1
    then edit1
    else edit2

drawUI :: St -> [T.Widget Name]
drawUI st = [ui]
    where
        ui = C.center $ (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 $ E.renderEditor $ st^.edit1)) <=>
                        str " " <=>
                        (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 $ E.renderEditor $ st^.edit2)) <=>
                        str " " <=>
                        str "Press Tab to switch between editors, Esc to quit."

appEvent :: St -> V.Event -> T.EventM Name (T.Next St)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ switchEditors st
        V.EvKey V.KBackTab [] -> M.continue $ switchEditors st
        _ -> M.continue =<<
          T.handleEventLensed st (currentEditorL st) E.handleEditorEvent ev

initialState :: St
initialState =
    St Edit1
       (E.editor Edit1 (str . unlines) Nothing "")
       (E.editor Edit2 (str . unlines) (Just 2) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    ]

appCursor :: St -> [T.CursorLocation Name] -> Maybe (T.CursorLocation Name)
appCursor st = M.showCursorNamed (st^.currentEditor)

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
