{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid
import Graphics.Vty hiding (translate)

import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Dialog
import Brick.AttrMap
import Brick.Util
import Brick.Types

data Choice = Red | Blue | Green
            deriving Show

drawUI :: Dialog Choice -> [Widget]
drawUI d = [ui]
    where
        ui = renderDialog d $ str "This is the dialog body."

appEvent :: Dialog Choice -> Event -> EventM (Next (Dialog Choice))
appEvent d ev =
    case ev of
        EvKey KEsc [] -> halt d
        EvKey KEnter [] -> halt d
        _ -> continue $ handleEvent ev d

initialState :: Dialog Choice
initialState = dialog "dialog" (Just "Title") (Just (0, choices)) 50
    where
        choices = [ ("Red", Red)
                  , ("Blue", Blue)
                  , ("Green", Green)
                  ]

theMap :: AttrMap
theMap = attrMap defAttr
    [ (dialogAttr, white `on` blue)
    , (buttonAttr, black `on` white)
    , (buttonSelectedAttr, bg yellow)
    ]

theApp :: App (Dialog Choice) Event
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const theMap
        , appMakeVtyEvent = id
        }

main :: IO ()
main = do
    d <- defaultMain theApp initialState
    putStrLn $ "You chose: " <> show (dialogSelection d)
