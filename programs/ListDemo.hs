{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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

data St =
    St { _stList :: List Int
       }

makeLenses ''St

drawUI :: St -> [Widget]
drawUI st = [ui]
    where
        label = "Item " <+> cur <+> " of " <+> total
        cur = case st^.stList.listSelectedL of
                Nothing -> "-"
                Just i -> str (show (i + 1))
        total = str $ show $ length $ st^.stList.listElementsL
        ui = center $
             borderWithLabel label $
             hLimit 25 $
             vLimit 15 $
             renderList (st^.stList)

appEvent :: Event -> St -> EventM (Next St)
appEvent e st =
    case e of
        EvKey KEnter [] ->
            let el = length $ st^.stList.listElementsL
            in continue $ st & stList %~ (listInsert el el)

        EvKey KEsc [] -> halt st

        ev -> continue $ st & stList %~ (handleEvent ev)

listDrawElement :: Bool -> Int -> Widget
listDrawElement sel i =
    let selStr s = if sel
                   then withAttrName customAttr (str $ "<" <> s <> ">")
                   else str s
    in hCenterWith (Just ' ') $ vBox $ for [1..i+1] $ \j ->
        "Item " <+> (selStr $ show i) <+> " Line " <+> (str $ show j)

initialState :: St
initialState =
    St { _stList = list (Name "list") listDrawElement [1, 2, 3]
       }

customAttr :: AttrName
customAttr = listSelectedAttr <> "custom"

theMap :: AttrMap
theMap = attrMap defAttr
    [ (listAttr,              white `on` blue)
    , (listSelectedAttr,      blue `on` white)
    , (customAttr,            fg cyan)
    ]

theApp :: App St Event
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appAttrMap = const theMap
        , appMakeVtyEvent = id
        }

main :: IO ()
main = void $ defaultMain theApp initialState
