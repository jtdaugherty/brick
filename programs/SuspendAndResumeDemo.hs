{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import Brick.Main
  ( App(..), neverShowCursor, defaultMain
  , suspendAndResume, halt
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , EventM
  , BrickEvent(..)
  )
import Brick.Widgets.Core
  ( vBox
  , str
  )

data St =
    St { _stExternalInput :: String
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [ui]
    where
        ui = vBox [ str $ "External input: \"" <> st^.stExternalInput <> "\""
                  , str "(Press Esc to quit or Space to ask for input)"
                  ]

appEvent :: BrickEvent () e -> EventM () St ()
appEvent (VtyEvent e) =
    case e of
        V.EvKey V.KEsc [] -> halt
        V.EvKey (V.KChar ' ') [] -> suspendAndResume $ do
            putStrLn "Suspended. Please enter something and press enter to resume:"
            s <- getLine
            return $ St { _stExternalInput = s }
        _ -> return ()
appEvent _ = return ()

initialState :: St
initialState =
    St { _stExternalInput = ""
       }

theApp :: App St e ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

main :: IO ()
main =
    void $ defaultMain theApp initialState
