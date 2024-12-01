{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import Brick.BChan
import Brick.Main
  ( App(..)
  , showFirstCursor
  , customMainWithDefaultVty
  , halt
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , EventM
  , BrickEvent(..)
  )
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core
  ( str
  , vBox
  , hBox
  , hLimit
  , vLimit
  )
import qualified Brick.Animation as A

data CustomEvent =
    AnimationUpdate (EventM () St ())

data St =
    St { _stAnimationManager :: A.AnimationManager St CustomEvent ()
       , _animation1 :: Maybe A.Animation
       , _animation2 :: Maybe A.Animation
       , _animation3 :: Maybe A.Animation
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [drawAnimations st]

drawAnimations :: St -> Widget ()
drawAnimations st =
    vBox [ hBox [ drawAnimation frames1 $ st^.animation1
                , str " "
                , drawAnimation frames2 $ st^.animation2
                , str " "
                , drawAnimation frames3 $ st^.animation3
                ]
         , vBox [ maybe (str " ") (const $ str "Animation #1 running") $ st^.animation1
                , maybe (str " ") (const $ str "Animation #2 running") $ st^.animation2
                , maybe (str " ") (const $ str "Animation #3 running") $ st^.animation3
                ]
         ]

-- NOTE:
-- Perhaps introduce a Frames type here with a Vector to store frames
-- for more efficient indexing
frames1 :: [Widget ()]
frames1 = str <$> [".", "o", "O", "^", " "]

frames2 :: [Widget ()]
frames2 = str <$> ["|", "/", "-", "\\"]

frames3 :: [Widget ()]
frames3 =
    (hLimit 9 . vLimit 9 . border . center) <$>
    [ border $ str " "
    , border $ vBox $ replicate 3 $ str $ replicate 3 ' '
    , border $ vBox $ replicate 5 $ str $ replicate 5 ' '
    ]

drawAnimation :: [Widget n] -> Maybe A.Animation -> Widget n
drawAnimation _ Nothing = str " "
drawAnimation frames (Just a) = frames !! (A.animationFrame a)

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e = do
    mgr <- use stAnimationManager
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey (V.KChar '1') []) -> do
            mOld <- use animation1
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr (length frames1) 1000 A.Forward A.Loop animation1

        VtyEvent (V.EvKey (V.KChar '2') []) -> do
            mOld <- use animation2
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr (length frames2) 100 A.Forward A.Loop animation2

        VtyEvent (V.EvKey (V.KChar '3') []) -> do
            mOld <- use animation3
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr (length frames3) 300 A.Forward A.Loop animation3

        AppEvent (AnimationUpdate act) -> act
        _ -> return ()

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

main :: IO ()
main = do
    chan <- newBChan 10
    mgr <- A.startAnimationManager chan AnimationUpdate

    let initialState =
            St { _stAnimationManager = mgr
               , _animation1 = Nothing
               , _animation2 = Nothing
               , _animation3 = Nothing
               }

    (_, vty) <- customMainWithDefaultVty (Just chan) theApp initialState
    V.shutdown vty
