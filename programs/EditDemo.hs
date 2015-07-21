{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import Brick.Widgets.Core
  ( Widget
  , (<+>)
  , hLimit
  , vLimit
  , str
  )
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.AttrMap as A
import Brick.Util (on)

drawUI :: E.Editor -> [Widget]
drawUI e = [ui]
    where
        ui = C.center $ "Input: " <+> (hLimit 30 $ vLimit 5 $ E.renderEditor e)

appEvent :: E.Editor -> V.Event -> M.EventM (M.Next E.Editor)
appEvent e ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt e
        _ -> M.continue $ T.handleEvent ev e

initialState :: E.Editor
initialState = E.editor (T.Name "edit") (str . unlines) Nothing ""

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (E.editAttr, V.white `on` V.blue)
    ]

theApp :: M.App E.Editor V.Event
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          , M.appLiftVtyEvent = id
          }

main :: IO ()
main = do
    e <- M.defaultMain theApp initialState
    putStrLn "You entered:\n"
    putStrLn $ unlines $ E.getEditContents e
