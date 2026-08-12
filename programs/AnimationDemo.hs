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
import Brick.Main (App(..), neverShowCursor, customMain, halt)
import Brick.AttrMap (AttrName, AttrMap, attrMap, attrName)
import Brick.Types (Widget, EventM, BrickEvent(..), Location(..))
import Brick.Widgets.Border (border)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core ((<+>), str, vBox, hBox, hLimit, vLimit, translateBy, withDefAttr, emptyWidget, fill,
                           reportExtent, (<=>), relativeTo)
import qualified Brick.Animation as A
import qualified Brick.Animation.Transition as A

data Name =
    Anchor
    deriving (Eq, Show, Ord)

data CustomEvent =
    AnimationUpdate (EventM Name St ())
    -- ^ The state update constructor required by the animation API

data St =
    St { _stAnimationManager :: A.AnimationManager St CustomEvent Name
       -- ^ The animation manager that will run all of our animations
       , _animation1 :: Maybe (A.Animation St Name)
       , _animation2 :: Maybe (A.Animation St Name)
       , _animation3 :: Maybe (A.Animation St Name)
       , _clickAnimations :: M.Map Location (A.Animation St Name)
       -- ^ The various fields for storing animation states. For mouse
       -- animations, we store animations for each screen location that
       -- was clicked.
       , _transition :: Maybe (A.Transition St Name)
       }

makeLenses ''St

drawUI :: St -> [Widget Name]
drawUI st =
    [A.renderTransition (const emptyWidget) st (st^.transition)] <>
    drawClickAnimations st <> [drawAnimations st, fill ' ']

drawClickAnimations :: St -> [Widget Name]
drawClickAnimations st =
    drawClickAnimation st <$> M.toList (st^.clickAnimations)

drawClickAnimation :: St -> (Location, A.Animation St Name) -> Widget Name
drawClickAnimation st (l, a) =
    translateBy l $
    A.renderAnimation (const $ str " ") st (Just a)

drawAnimations :: St -> Widget Name
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
    in vBox [ str "Click and drag the mouse or press keys to start animations."
            , str " "
            , vBox statusMessages
            , animationDrawings
            ]

clip1 :: A.Clip a Name
clip1 = A.newClip_ $ str <$> [".", "o", "O", "^", " "]

clip2 :: A.Clip a Name
clip2 = A.newClip_ $ str <$> ["|", "/", "-", "\\"]

clip3 :: A.Clip a Name
clip3 =
    A.newClip_ $
    (hLimit 9 . vLimit 9 . border . center) <$>
    [ border $ str " "
    , border $ vBox $ replicate 3 $ str $ replicate 3 ' '
    , border $ vBox $ replicate 5 $ str $ replicate 5 ' '
    ]

mouseClickClip :: A.Clip a Name
mouseClickClip =
    A.newClip_
    [ withDefAttr attr6 $ str "0"
    , withDefAttr attr5 $ str "O"
    , withDefAttr attr4 $ str "o"
    , withDefAttr attr3 $ str "•"
    , withDefAttr attr2 $ str "*"
    , withDefAttr attr2 $ str "."
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

-- | Animation settings grouped together for lookup by keystroke.
data AnimationConfig =
    AnimationConfig { animationTarget :: Lens' St (Maybe (A.Animation St Name))
                    , animationClip :: A.Clip St Name
                    , animationFrameTime :: Integer
                    , animationMode :: A.RunMode
                    }

animations :: [(Char, AnimationConfig)]
animations =
    [ ('1', AnimationConfig animation1 clip1 1000 A.Loop)
    , ('2', AnimationConfig animation2 clip2 100 A.Loop)
    , ('3', AnimationConfig animation3 clip3 100 A.Once)
    ]

-- | Start the animation specified by this config.
startAnimationFromConfig :: AnimationConfig -> EventM Name St ()
startAnimationFromConfig config = do
    mgr <- use stAnimationManager
    A.startAnimation mgr (animationClip config)
                         (animationFrameTime config)
                         (animationMode config)
                         (animationTarget config)

-- | If the animation specified in this config is not running, start it.
-- Otherwise stop it.
toggleAnimationFromConfig :: AnimationConfig -> EventM Name St ()
toggleAnimationFromConfig config = do
    mgr <- use stAnimationManager
    mOld <- use (animationTarget config)
    case mOld of
        Just a -> A.stopAnimation mgr a
        Nothing -> startAnimationFromConfig config

-- | Start a new mouse click animation at the specified location if one
-- is not already running there.
startMouseClickAnimation :: Location -> EventM Name St ()
startMouseClickAnimation l = do
    mgr <- use stAnimationManager
    a <- use (clickAnimations.at l)
    case a of
        Just {} -> return ()
        Nothing -> A.startAnimation mgr mouseClickClip 100 A.Once (clickAnimations.at l)

slideUpBox :: Widget Name
slideUpBox =
    border $
    vBox $
    str <$> ["line " <> show i | i <- [1..20::Int]]

appEvent :: BrickEvent Name CustomEvent -> EventM Name St ()
appEvent e = do
    case e of
        -- A mouse click starts an animation at the click location.
        VtyEvent (V.EvMouseDown col row _ _) ->
            startMouseClickAnimation (Location (col, row))

        -- If we got a character keystroke, see if there is a specific
        -- animation mapped to that character and toggle the resulting
        -- animation.
        VtyEvent (V.EvKey (V.KChar c) [])
            | Just aConfig <- lookup c animations ->
                toggleAnimationFromConfig aConfig

        VtyEvent (V.EvKey (V.KChar 't') []) -> do
            mgr <- use stAnimationManager
            A.stopTransition mgr transition
            A.startTransition mgr (const slideUpBox) 20 10 A.slideUp transition

        -- Apply a state update from the animation manager.
        AppEvent (AnimationUpdate act) -> act

        VtyEvent (V.EvKey V.KEsc []) -> halt

        _ -> return ()

theApp :: App St CustomEvent Name
theApp =
    App { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const attrs
        }

main :: IO ()
main = do
    chan <- newBChan 10
    mgr <- A.startAnimationManager 30 chan AnimationUpdate

    let initialState =
            St { _stAnimationManager = mgr
               , _animation1 = Nothing
               , _animation2 = Nothing
               , _animation3 = Nothing
               , _clickAnimations = mempty
               , _transition = Nothing
               }
        buildVty = do
            v <- mkVty V.defaultConfig
            V.setMode (V.outputIface v) V.Mouse True
            return v

    initialVty <- buildVty
    void $ customMain initialVty buildVty (Just chan) theApp initialState
