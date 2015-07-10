{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (.~), (^.), (&))
import Control.Monad (void)
import Data.Monoid
import Data.Default
import qualified Graphics.Vty as V

import Brick.Main
  ( App(..), neverShowCursor, defaultMain
  , suspendAndResume, halt, continue
  , EventM, Next
  )
import Brick.Widgets.Core
  ( Widget
  , vBox
  , str
  )

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

appEvent :: St -> V.Event -> EventM (Next St)
appEvent st e =
    case e of
        V.EvKey V.KEsc [] -> halt st
        V.EvKey (V.KChar ' ') [] -> suspendAndResume $ do
            -- NB: https://github.com/coreyoconnor/vty/issues/77
            putStrLn "Suspended. Please enter something and press enter to resume:"
            s <- getLine
            return $ st & stExternalInput .~ s
        _ -> continue st

initialState :: St
initialState =
    St { _stExternalInput = ""
       }

theApp :: App St V.Event
theApp =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return
        , appAttrMap = const def
        , appLiftVtyEvent = id
        }

main :: IO ()
main = do
    void $ defaultMain theApp initialState
