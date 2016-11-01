{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( padAll
  , str
  )
import qualified Brick.Widgets.Dialog as D
import qualified Brick.Widgets.Center as C
import qualified Brick.AttrMap as A
import Brick.Util (on, bg)
import qualified Brick.Types as T

data Choice = Red | Blue | Green
            deriving Show

drawUI :: D.Dialog Choice -> [Widget ()]
drawUI d = [ui]
    where
        ui = D.renderDialog d $ C.hCenter $ padAll 1 $ str "This is the dialog body."

appEvent :: D.Dialog Choice -> BrickEvent () e -> T.EventM () (T.Next (D.Dialog Choice))
appEvent d (VtyEvent ev) =
    case ev of
        V.EvKey V.KEsc [] -> M.halt d
        V.EvKey V.KEnter [] -> M.halt d
        _ -> M.continue =<< D.handleDialogEvent ev d
appEvent d _ = M.continue d

initialState :: D.Dialog Choice
initialState = D.dialog (Just "Title") (Just (0, choices)) 50
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

theApp :: M.App (D.Dialog Choice) e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    d <- M.defaultMain theApp initialState
    putStrLn $ "You chose: " <> show (D.dialogSelection d)
