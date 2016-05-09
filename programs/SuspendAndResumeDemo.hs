{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((.~), (^.), (&))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Monoid
import Data.Default
import qualified Graphics.Vty as V

import Brick.Main
  ( App(..), neverShowCursor, defaultMain
  , suspendAndResume, halt, continue
  )
import Brick.Types
  ( Widget
  , EventM
  , Next
  )
import Brick.Widgets.Core
  ( vBox
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
                  , str "(Press Esc to quit or Space to ask for input)"
                  ]

appEvent :: St -> V.Event -> EventM (Next St)
appEvent st e =
    case e of
        V.EvKey V.KEsc [] -> halt st
        V.EvKey (V.KChar ' ') [] -> suspendAndResume $ do
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
main =
    void $ defaultMain theApp initialState
