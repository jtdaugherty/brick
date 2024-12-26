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
import Brick.Util (fg)
import Brick.Main (App(..), showFirstCursor, customMain, halt)
import Brick.AttrMap (AttrName, AttrMap, attrMap, attrName)
import Brick.Types (Widget, EventM, BrickEvent(..), Location(..))
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core ((<+>), str, vBox, hBox, hLimit, vLimit, translateBy, withDefAttr)
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
    A.renderAnimation (const $ str " ") st (Just a)

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
                            drawSingleAnimation <$> animations
        drawSingleAnimation (_, config) =
            A.renderAnimation (const $ str " ") st (st^.(animationTarget config))
    in vBox $
       str "Click and drag the mouse or press keys to start animations." :
       str " " :
       statusMessages <> [animationDrawings]

clip1 :: A.Clip a ()
clip1 = A.newClip_ $ str <$> [".", "o", "O", "^", " "]

clip2 :: A.Clip a ()
clip2 = A.newClip_ $ str <$> ["|", "/", "-", "\\"]

clip3 :: A.Clip a ()
clip3 =
    A.newClip_ $
    (hLimit 9 . vLimit 9 . border . center) <$>
    [ border $ str " "
    , border $ vBox $ replicate 3 $ str $ replicate 3 ' '
    , border $ vBox $ replicate 5 $ str $ replicate 5 ' '
    ]

mouseClickClip :: A.Clip a ()
mouseClickClip =
    A.newClip_ $
    [ withDefAttr attr6 $ str "0"
    , withDefAttr attr5 $ str "O"
    , withDefAttr attr4 $ str "o"
    , withDefAttr attr3 $ str "*"
    , withDefAttr attr2 $ str "~"
    , withDefAttr attr1 $ str "."
    ]

attr6 :: AttrName
attr6 = attrName "attr6"

attr5 :: AttrName
attr5 = attrName "attr5"

attr4 :: AttrName
attr4 = attrName "attr4"

attr3 :: AttrName
attr3 = attrName "attr3"

attr2 :: AttrName
attr2 = attrName "attr2"

attr1 :: AttrName
attr1 = attrName "attr1"

attrs :: AttrMap
attrs =
    attrMap V.defAttr
        [ (attr6, fg V.white)
        , (attr5, fg V.brightYellow)
        , (attr4, fg V.brightGreen)
        , (attr3, fg V.cyan)
        , (attr2, fg V.blue)
        , (attr1, fg V.black)
        ]

data AnimationConfig =
    AnimationConfig { animationTarget :: Lens' St (Maybe (A.Animation St ()))
                    , animationClip :: A.Clip St ()
                    , animationFrameTime :: Integer
                    , animationMode :: A.RunMode
                    }

animations :: [(Char, AnimationConfig)]
animations =
    [ ('1', AnimationConfig animation1 clip1 1000 A.Loop)
    , ('2', AnimationConfig animation2 clip2 100 A.Loop)
    , ('3', AnimationConfig animation3 clip3 100 A.Once)
    ]

startAnimationFromConfig :: AnimationConfig -> EventM () St ()
startAnimationFromConfig config = do
    mgr <- use stAnimationManager
    A.startAnimation mgr (animationClip config)
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

startMouseClickAnimation :: Location -> EventM () St ()
startMouseClickAnimation l = do
    mgr <- use stAnimationManager
    A.startAnimation mgr mouseClickClip 100 A.Once (clickAnimations.at l)

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e = do
    case e of
        VtyEvent (V.EvMouseDown col row _ _) ->
            startMouseClickAnimation (Location (col, row))

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
        , appAttrMap = const attrs
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
