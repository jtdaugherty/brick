{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad (void)
import Lens.Micro.Platform
import Data.List (intersperse)
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
        statusMessages = statusMessage <$> zip [(0::Int)..] animations
        statusMessage (i, (c, config)) =
            animStatus ("Animation #" <> (show $ i + 1)) [c]
                       (st^.(animationTarget config))
        animationDrawings = hBox $ intersperse (str " ") $
                            drawSingle <$> animations
        drawSingle (_, config) =
            A.renderAnimation (str " ") st (st^.(animationTarget config))
    in vBox $ statusMessages <> [animationDrawings]

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

toggleMouseClickAnimation :: Location -> EventM () St ()
toggleMouseClickAnimation l = do
    -- If an animation is already running at this location, stop it;
    -- else start a new one.
    mgr <- use stAnimationManager
    mA <- use (clickAnimations.at l)
    case mA of
        Nothing -> A.startAnimation mgr (frames2 <> frames2) 100 A.Once (clickAnimations.at l)
        Just a -> A.stopAnimation mgr a

data AnimationConfig =
    AnimationConfig { animationTarget :: Lens' St (Maybe (A.Animation St ()))
                    , animationFrames :: A.FrameSeq St ()
                    , animationFrameTime :: Integer
                    , animationMode :: A.RunMode
                    }

animations :: [(Char, AnimationConfig)]
animations =
    [ ('1', AnimationConfig animation1 frames1 1000 A.Loop)
    , ('2', AnimationConfig animation2 frames2 100 A.Loop)
    , ('3', AnimationConfig animation3 frames3 100 A.Once)
    ]

startAnimationFromConfig :: AnimationConfig -> EventM () St ()
startAnimationFromConfig config = do
    mgr <- use stAnimationManager
    A.startAnimation mgr (animationFrames config)
                         (animationFrameTime config)
                         (animationMode config)
                         (animationTarget config)

toggleAnimationFromConfig :: AnimationConfig -> EventM () St ()
toggleAnimationFromConfig config = do
    mgr <- use stAnimationManager
    mOld <- use (animationTarget config)
    case mOld of
        Just a -> A.stopAnimation mgr a
        Nothing -> startAnimationFromConfig config

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e = do
    case e of
        VtyEvent (V.EvMouseDown col row _ _) -> do
            toggleMouseClickAnimation (Location (col, row))

        VtyEvent (V.EvKey (V.KChar c) [])
            | Just aConfig <- lookup c animations ->
                toggleAnimationFromConfig aConfig

        AppEvent (AnimationUpdate act) -> act

        VtyEvent (V.EvKey V.KEsc []) -> halt

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
