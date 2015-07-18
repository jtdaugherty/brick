{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Widgets.Core
  ( Widget
  , padAll
  , str
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

data Choice = Red | Blue | Green
            deriving Show

drawUI :: D.Dialog Choice -> [Widget]
drawUI d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the dialog body."

appEvent :: D.Dialog Choice -> V.Event -> M.EventM (M.Next (D.Dialog Choice))
appEvent d ev =
    case ev of
        V.EvKey V.KEsc [] -> M.halt d
        V.EvKey V.KEnter [] -> M.halt d
        _ -> M.continue $ T.handleEvent ev d

initialState :: D.Dialog Choice
initialState = D.dialog "dialog" (Just "Title") (Just (0, choices)) 50
    where
        choices = [ ("Red", Red)
                  , ("Blue", Blue)
                  , ("Green", Green)
                  ]

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (D.dialogAttr, V.white `on` V.blue)
    , (D.buttonAttr, V.black `on` V.white)
    , (D.buttonSelectedAttr, bg V.yellow)
    ]

theApp :: M.App (D.Dialog Choice) V.Event
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
    d <- M.defaultMain theApp initialState
    putStrLn $ "You chose: " <> show (D.dialogSelection d)
