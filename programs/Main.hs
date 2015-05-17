{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Default
import Graphics.Vty
import System.Exit

import Brick

data St =
    St { _stEditor :: Editor
       }

makeLenses ''St

drawUI :: St -> [Prim]
drawUI st = [a]
    where
        a = centered $
            bordered $
            VLimit 1 $
            HLimit 25 $
            UseAttr (cyan `on` blue) $
            edit (st^.stEditor)

handleEvent :: Event -> St -> IO St
handleEvent e st =
    case e of
        EvKey KEsc [] -> exitSuccess
        EvKey KEnter [] -> error $ editStr $ st^.stEditor
        ev -> return $ st & stEditor %~ (editEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (Name "edit") ""
       }

theApp :: App St Event
theApp =
    def { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appHandleResize =
            \_ sz st -> st & stEditor %~ setSize sz
        }

main :: IO ()
main = defaultMain theApp initialState
