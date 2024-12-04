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
  ( (<+>)
  , str
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
       , _animation1 :: Maybe (A.Animation St ())
       , _animation2 :: Maybe (A.Animation St ())
       , _animation3 :: Maybe (A.Animation St ())
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [drawAnimations st]

drawAnimations :: St -> Widget ()
drawAnimations st =
    let animStatus label a =
            str (label <> ": ") <+>
            maybe (str "Not running") (const $ str "Running") a
    in vBox [ animStatus "Animation #1" (st^.animation1)
            , animStatus "Animation #2" (st^.animation2)
            , animStatus "Animation #3" (st^.animation3)
            , hBox [ A.drawAnimation (str " ") st $ st^.animation1
                   , str " "
                   , A.drawAnimation (str " ") st $ st^.animation2
                   , str " "
                   , A.drawAnimation (str " ") st $ st^.animation3
                   ]
            ]

frames1 :: A.Frames St ()
frames1 = A.newFrames $ (const . str) <$> [".", "o", "O", "^", " "]

frames2 :: A.Frames St ()
frames2 = A.newFrames $ (const . str) <$> ["|", "/", "-", "\\"]

frames3 :: A.Frames St ()
frames3 =
    A.newFrames $
    (const . hLimit 9 . vLimit 9 . border . center) <$>
    [ border $ str " "
    , border $ vBox $ replicate 3 $ str $ replicate 3 ' '
    , border $ vBox $ replicate 5 $ str $ replicate 5 ' '
    ]

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e = do
    mgr <- use stAnimationManager
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey (V.KChar '1') []) -> do
            mOld <- use animation1
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr frames1 1000 A.Forward A.Loop animation1

        VtyEvent (V.EvKey (V.KChar '2') []) -> do
            mOld <- use animation2
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr frames2 100 A.Forward A.Loop animation2

        VtyEvent (V.EvKey (V.KChar '3') []) -> do
            mOld <- use animation3
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr frames3 300 A.Forward A.Loop animation3

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
