{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad (void)
import Data.Monoid
import Data.Default
import Graphics.Vty hiding (translate)

import Brick.Main
import Brick.Widgets.Core

data St =
    St { _stExternalInput :: String
       }

makeLenses ''St

drawUI :: St -> [Widget]
drawUI st = [ui]
    where
        ui = vBox [ str $ "External input: \"" <> st^.stExternalInput <> "\""
                  , "(Press Esc to quit or Space to ask for input)"
                  ]

appEvent :: Event -> St -> EventM (Next St)
appEvent e st =
    case e of
        EvKey KEsc [] -> halt st
        EvKey (KChar ' ') [] -> suspendAndResume $ do
            -- NB: https://github.com/coreyoconnor/vty/issues/77
            putStrLn "Suspended. Please enter something and press enter to resume:"
            s <- getLine
            return $ st & stExternalInput .~ s
        _ -> continue st

initialState :: St
initialState =
    St { _stExternalInput = ""
       }

theApp :: App St Event
theApp =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const def
        , appMakeVtyEvent = id
        }

main :: IO ()
main = do
    void $ defaultMain theApp initialState
    putStrLn "Enter a line:"
    s <- getLine
    print s
