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
       , _trans :: Location
       }

makeLenses ''St

drawUI :: St -> [Prim]
drawUI st = [a]
    where
        a = centered box
        box = bordered $ VLimit 1 $ HLimit 25 $ UseAttr (cyan `on` blue) $ edit (st^.stEditor)

handleEvent :: Event -> St -> IO St
handleEvent e st =
    case e of
        EvKey KEsc []         -> exitSuccess
        EvKey KEnter []       -> error $ editStr $ st^.stEditor
        EvKey KLeft [MCtrl]   -> return $ st & trans %~ (<> (Location (-1, 0)))
        EvKey KRight [MCtrl]  -> return $ st & trans %~ (<> (Location (1, 0)))
        EvKey KUp [MCtrl]     -> return $ st & trans %~ (<> (Location (0, -1)))
        EvKey KDown [MCtrl]   -> return $ st & trans %~ (<> (Location (0, 1)))
        ev                    -> return $ st & stEditor %~ (editEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (Name "edit") ""
       , _trans = Location (0, 0)
       }

theApp :: App St Event
theApp =
    def { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = handleEvent
        , appHandleResize =
            \name sz st -> st & stEditor %~ resizeEdit sz
        }

main :: IO ()
main = defaultMain theApp initialState
