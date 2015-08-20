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

data St =
    St { _currentEditor :: T.Name
       , _edit1 :: E.Editor
       , _edit2 :: E.Editor
       }

makeLenses ''St

firstEditor :: T.Name
firstEditor = "edit1"

secondEditor :: T.Name
secondEditor = "edit2"

switchEditors :: St -> St
switchEditors st =
    let next = if st^.currentEditor == firstEditor
               then secondEditor else firstEditor
    in st & currentEditor .~ next

currentEditorL :: St -> Lens' St E.Editor
currentEditorL st =
    if st^.currentEditor == firstEditor
    then edit1
    else edit2

drawUI :: St -> [T.Widget]
drawUI st = [ui]
    where
        ui = C.center $ (str "Input 1 (unlimited): " <+> (hLimit 30 $ vLimit 5 $ E.renderEditor $ st^.edit1)) <=>
                        str " " <=>
                        (str "Input 2 (limited to 2 lines): " <+> (hLimit 30 $ E.renderEditor $ st^.edit2)) <=>
                        str " " <=>
                        str "Press Tab to switch between editors, Esc to quit."

appEvent :: St -> V.Event -> T.EventM (T.Next St)
appEvent st ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt st
        V.EvKey (V.KChar '\t') [] -> M.continue $ switchEditors st
        _ -> M.continue =<< T.handleEventLensed st (currentEditorL st) ev

initialState :: St
initialState =
    St firstEditor
       (E.editor firstEditor (str . unlines) Nothing "")
       (E.editor secondEditor (str . unlines) (Just 2) "")

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    ]

appCursor :: St -> [T.CursorLocation] -> Maybe T.CursorLocation
appCursor st = M.showCursorNamed (st^.currentEditor)

theApp :: M.App St V.Event
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
