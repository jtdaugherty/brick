{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Default
import Data.Monoid
import Graphics.Vty
import System.Exit

import Brick

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
             edit (st^.stEditor))
            <<=
            HFill '-'
            =>>
            (VLimit 10 $
             HLimit 25 $
             list (st^.stList))

appEvent :: Event -> St -> IO St
appEvent e st =
    case e of
        EvKey KEsc [] -> exitSuccess
        EvKey KEnter [] -> error $ editStr $ st^.stEditor
        ev -> return $ st & stEditor %~ (handleEvent ev)
                          & stList %~ (handleEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (Name "edit") ""
       , _stList = newList (Name "list") listDrawElem [0..6]
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
