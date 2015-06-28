{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Monoid
import Graphics.Vty hiding (translate)

import Brick.Main
import Brick.Core
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Edit
import Brick.AttrMap
import Brick.Util

data St =
    St { _stEditor :: Editor
       }

makeLenses ''St

drawUI :: St -> [Widget]
drawUI st = [ui]
    where
        ui = center $ ("Input: " <+> (hLimit 30 $ renderEditor $ st^.stEditor))

appEvent :: Event -> St -> EventM (Next St)
appEvent e st =
    case e of
        EvKey KEsc [] -> halt st
        EvKey KEnter [] -> halt st
        ev -> continue $ st & stEditor %~ (handleEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (Name "edit") str ""
       }

theMap :: AttrMap
theMap = attrMap defAttr
    [ (editAttr, white `on` blue)
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
main = do
    st <- defaultMain theApp initialState
    putStrLn $ "You entered: " <> (st^.stEditor.editContentsL)
