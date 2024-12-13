{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Monad (void)
import Lens.Micro.Platform
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Data.Map as M
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)

import Brick.BChan
import Brick.Main
  ( App(..)
  , showFirstCursor
  , customMain
  , halt
  )
import Brick.AttrMap
  ( attrMap
  )
import Brick.Types
  ( Widget
  , EventM
  , BrickEvent(..)
  , Location(..)
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
  , translateBy
  )
import qualified Brick.Animation as A

data CustomEvent =
    AnimationUpdate (EventM () St ())

data St =
    St { _stAnimationManager :: A.AnimationManager St CustomEvent ()
       , _animation1 :: Maybe (A.Animation St ())
       , _animation2 :: Maybe (A.Animation St ())
       , _animation3 :: Maybe (A.Animation St ())
       , _clickAnimations :: M.Map Location (A.Animation St ())
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = drawClickAnimations st <> [drawAnimations st]

drawClickAnimations :: St -> [Widget ()]
drawClickAnimations st =
    drawClickAnimation st <$> M.toList (st^.clickAnimations)

drawClickAnimation :: St -> (Location, A.Animation St ()) -> Widget ()
drawClickAnimation st (l, a) =
    translateBy l $
    A.renderAnimation (str " ") st (Just a)

drawAnimations :: St -> Widget ()
drawAnimations st =
    let animStatus label key a =
            str (label <> ": ") <+>
            maybe (str "Not running") (const $ str "Running") a <+>
            str (" (Press " <> key <> " to toggle)")
    in vBox [ animStatus "Animation #1" "1" (st^.animation1)
            , animStatus "Animation #2" "2" (st^.animation2)
            , animStatus "Animation #3" "3" (st^.animation3)
            , hBox [ A.renderAnimation (str " ") st $ st^.animation1
                   , str " "
                   , A.renderAnimation (str " ") st $ st^.animation2
                   , str " "
                   , A.renderAnimation (str " ") st $ st^.animation3
                   ]
            ]

frames1 :: A.FrameSeq a ()
frames1 = A.newFrameSeq $ (const . str) <$> [".", "o", "O", "^", " "]

frames2 :: A.FrameSeq a ()
frames2 = A.newFrameSeq $ (const . str) <$> ["|", "/", "-", "\\"]

frames3 :: A.FrameSeq a ()
frames3 =
    A.newFrameSeq $
    (const . hLimit 9 . vLimit 9 . border . center) <$>
    [ border $ str " "
    , border $ vBox $ replicate 3 $ str $ replicate 3 ' '
    , border $ vBox $ replicate 5 $ str $ replicate 5 ' '
    ]

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e = do
    mgr <- use stAnimationManager
    case e of
        VtyEvent (V.EvMouseDown col row _ _) -> do
            -- If an animation is already running here, stop it; else
            -- start a new one.
            let l = Location (col, row)
            mA <- use (clickAnimations.at l)
            case mA of
                Nothing -> A.startAnimation mgr frames2 100 A.Loop (clickAnimations.at l)
                Just a -> A.stopAnimation mgr a
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey (V.KChar '1') []) -> do
            mOld <- use animation1
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr frames1 1000 A.Loop animation1

        VtyEvent (V.EvKey (V.KChar '2') []) -> do
            mOld <- use animation2
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr frames2 100 A.Loop animation2

        VtyEvent (V.EvKey (V.KChar '3') []) -> do
            mOld <- use animation3
            case mOld of
                Just a -> A.stopAnimation mgr a
                Nothing -> A.startAnimation mgr frames3 300 A.Loop animation3

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
    mgr <- A.startAnimationManager 100 chan AnimationUpdate

    let initialState =
            St { _stAnimationManager = mgr
               , _animation1 = Nothing
               , _animation2 = Nothing
               , _animation3 = Nothing
               , _clickAnimations = mempty
               }
        buildVty = do
            v <- mkVty V.defaultConfig
            V.setMode (V.outputIface v) V.Mouse True
            return v

    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) theApp initialState
