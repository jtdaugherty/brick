{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Default
import Data.Monoid
import Graphics.Vty
import System.Exit

import Brick.Main
import Brick.Edit
import Brick.List
import Brick.Core
import Brick.Prim
import Brick.Center
import Brick.Border
import Brick.Util

data St =
    St { _stEditor :: Editor
       , _stList :: List Int
       }

makeLenses ''St

drawUI :: St -> [Prim]
drawUI st = [a]
    where
        a = centered $
            bordered $
            (VLimit 1 $
             HLimit 25 $
             UseAttr (cyan `on` blue) $
             drawEditor (st^.stEditor))
            <<=
            HFill '-'
            =>>
            (VLimit 10 $
             HLimit 25 $
             drawList (st^.stList))

appEvent :: Event -> St -> IO St
appEvent e st =
    case e of
        EvKey KEsc [] -> exitSuccess
        EvKey KEnter [] ->
            let el = length $ listElements $ st^.stList
            in return $ st & stList %~ listInsert el el
        ev -> return $ st & stEditor %~ (handleEvent ev)
                          & stList %~ (handleEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (Name "edit") ""
       , _stList = list (Name "list") listDrawElem []
       }

listDrawElem :: Bool -> Int -> Prim
listDrawElem sel i =
    let selAttr = white `on` blue
        p = hCentered (Txt $ "Number " <> show i)
    in if sel
       then UseAttr selAttr p
       else p

theApp :: App St Event
theApp =
    def { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appHandleSize =
            \name sz st ->
                case name of
                    Name "edit" -> st & stEditor %~ setSize sz
                    Name "list" -> st & stList %~ setSize sz
                    _ -> st
        }

main :: IO ()
main = defaultMain theApp initialState
