{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import Lens.Micro ((^.), Traversal')
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void, forever)
import Control.Concurrent (threadDelay, forkIO, ThreadId)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import qualified Control.Concurrent.STM as STM
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
import Brick.Widgets.Core
  ( (<=>)
  , str
  )

data CustomEvent = Counter deriving Show

data St =
    St { _stLastBrickEvent :: Maybe (BrickEvent () CustomEvent)
       , _stCounter :: Int
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [a]
    where
        a = (str $ "Last event: " <> (show $ st^.stLastBrickEvent))
            <=>
            (str $ "Counter value is: " <> (show $ st^.stCounter))

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e =
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent _ -> stLastBrickEvent .= (Just e)
        AppEvent Counter -> do
            stCounter %= (+1)
            stLastBrickEvent .= (Just e)
        _ -> return ()

initialState :: St
initialState =
    St { _stLastBrickEvent = Nothing
       , _stCounter = 0
       }

theApp :: App St CustomEvent ()
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap V.defAttr []
        }

data AnimationManagerRequest s =
    Tick
    | StartAnimation (Animation s)
    | StopAnimation AnimationID
    | Shutdown

data Duration =
    Infinite
    | Loop Int
    deriving (Eq, Show, Ord)

data AnimationMode =
    Forward
    -- | Backward
    -- | PingPong
    -- | Random
    deriving (Eq, Show, Ord)

data Animation s =
    Animation { animationID :: AnimationID
              , animationNumFrames :: Int
              , animationCurrentFrame :: Int
              , animationPreviousFrame :: Maybe Int
              -- what about tracking that an animation is currently
              -- moving backward when it sometimes moves forward? Just
              -- track the previous frame always, and use that? that
              -- works in general (can be ignored in the random case but
              -- is used in all others)
              , animationMode :: AnimationMode
              , animationDuration :: Duration
              , animationFrameUpdater :: Traversal' s (Maybe Int)
              }

newtype AnimationID = AnimationID Int
           deriving (Eq, Ord, Show, Hashable)

data AnimationManager s e n =
    AnimationManager { animationMgrRequestThreadId :: ThreadId
                     , animationMgrTickThreadId :: ThreadId
                     , animationMgrOutputChan :: BChan e
                     , animationMgrInputChan :: STM.TChan (AnimationManagerRequest s)
                     , animationMgrEventConstructor :: EventM n s () -> e
                     , animationMgrMillisecondsPerTick :: Int
                     , animationMgrNextAnimationID :: STM.TVar AnimationID
                     }

tickThreadBody :: Int
               -> STM.TChan (AnimationManagerRequest s)
               -> IO ()
tickThreadBody msPerTick outChan =
    forever $ do
        threadDelay $ msPerTick * 1000
        STM.atomically $ STM.writeTChan outChan Tick

animationManagerThreadBody :: STM.TChan (AnimationManagerRequest s)
                           -> BChan e
                           -> (EventM n s () -> e)
                           -> IO ()
animationManagerThreadBody inChan outChan mkEvent =
    let initialState :: HM.HashMap AnimationID (Animation s)
        initialState = mempty

        loop st = do
            req <- STM.atomically $ STM.readTChan inChan
            case req of
                StartAnimation a ->
                    loop $ HM.insert (animationID a) a st

                StopAnimation aId ->
                    -- TODO: update the application state here
                    loop $ HM.delete aId st

                Tick ->
                    -- Check all animation states for frame advances
                    return ()

                Shutdown ->
                    return ()

    in loop initialState

-- When a tick occurs:
--  for each currently-running animation,
--    check to see if the animation should advance and if so by how much
--      if it advances at all, schedule that animation state to be updated
-- if any animations have advanced, send an event to the application to
--   update the animation states involved and redraw
--
-- Meanwhile, we can also receive requests from the application to:
--
-- * start a new free-running animation
-- * start a manually-controlled animation
-- * remove an animation (effectively stopping it)
-- * shut down entirely

startAnimationManager :: Int -> BChan e -> (EventM n s () -> e) -> IO (AnimationManager s e n)
startAnimationManager msPerTick outChan mkEvent = do
    inChan <- STM.newTChanIO
    reqTid <- forkIO $ animationManagerThreadBody inChan outChan mkEvent
    tickTid <- forkIO $ tickThreadBody msPerTick inChan
    idVar <- STM.newTVarIO $ AnimationID 1
    return $ AnimationManager { animationMgrRequestThreadId = reqTid
                              , animationMgrTickThreadId = tickTid
                              , animationMgrEventConstructor = mkEvent
                              , animationMgrOutputChan = outChan
                              , animationMgrInputChan = inChan
                              , animationMgrMillisecondsPerTick = msPerTick
                              , animationMgrNextAnimationID = idVar
                              }

stopAnimationManager :: AnimationManager s e n -> IO ()
stopAnimationManager mgr = tellAnimationManager mgr Shutdown

tellAnimationManager :: AnimationManager s e n -> AnimationManagerRequest s -> IO ()
tellAnimationManager mgr req =
    STM.atomically $
    STM.writeTChan (animationMgrInputChan mgr) req

main :: IO ()
main = do
    chan <- newBChan 10

    void $ forkIO $ forever $ do
        writeBChan chan Counter
        threadDelay 1000000

    void $ customMainWithDefaultVty (Just chan) theApp initialState
