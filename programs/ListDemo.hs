{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Monad (void)
import Data.Monoid
import Graphics.Vty

import Brick.Main
import Brick.Core
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Widgets.List
import Brick.Widgets.Center
import Brick.AttrMap
import Brick.Util

drawUI :: List Int -> [Widget]
drawUI l = [ui]
    where
        label = "Item " <+> cur <+> " of " <+> total
        cur = case l^.listSelectedL of
                Nothing -> "-"
                Just i -> str (show (i + 1))
        total = str $ show $ length $ l^.listElementsL
        box = borderWithLabel label $
              hLimit 25 $
              vLimit 15 $
              renderList l
        ui = vCenter $ vBox [ hCenter box
                            , " "
                            , hCenter "Press +/- to add/remove list elements."
                            , hCenter "Press Esc to exit."
                            ]

appEvent :: List Int -> Event -> EventM (Next (List Int))
appEvent l e =
    case e of
        EvKey (KChar '+') [] ->
            let el = length $ l^.listElementsL
            in continue $ listInsert el el l

        EvKey (KChar '-') [] ->
            case l^.listSelectedL of
                Nothing -> continue l
                Just i -> continue $ listRemove i l

        EvKey KEsc [] -> halt l

        ev -> continue $ handleEvent ev l

listDrawElement :: Bool -> Int -> Widget
listDrawElement sel i =
    let selStr s = if sel
                   then withAttr customAttr (str $ "<" <> s <> ">")
                   else str s
    in hCenter $ "Item " <+> (selStr $ show i)

initialState :: List Int
initialState = list (Name "list") listDrawElement [0, 1, 2]

customAttr :: AttrName
customAttr = listSelectedAttr <> "custom"

theMap :: AttrMap
theMap = attrMap defAttr
    [ (listAttr,              white `on` blue)
    , (listSelectedAttr,      blue `on` white)
    , (customAttr,            fg cyan)
    ]

theApp :: App (List Int) Event
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const theMap
        , appMakeVtyEvent = id
        }

main :: IO ()
main = void $ defaultMain theApp initialState
