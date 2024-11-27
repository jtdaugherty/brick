{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lens.Micro ((^.), Traversal')
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void, forever, when)
import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread)
import Control.Monad.State.Strict
import Data.Hashable (Hashable)
import Data.Time.Clock (UTCTime, NominalDiffTime, addUTCTime, getCurrentTime)
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
    Tick UTCTime
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
              , animationFrameMilliseconds :: Integer
              -- what about tracking that an animation is currently
              -- moving backward when it sometimes moves forward? Just
              -- track the previous frame always, and use that? that
              -- works in general (can be ignored in the random case but
              -- is used in all others)
              , animationMode :: AnimationMode
              , animationDuration :: Duration
              , animationFrameUpdater :: Traversal' s (Maybe Int)
              , animationNextFrameTime :: UTCTime
              }

newtype AnimationID = AnimationID Int
           deriving (Eq, Ord, Show, Hashable)

data AnimationManager s e n =
    AnimationManager { animationMgrRequestThreadId :: ThreadId
                     , animationMgrTickThreadId :: ThreadId
                     , animationMgrOutputChan :: BChan e
                     , animationMgrInputChan :: STM.TChan (AnimationManagerRequest s)
                     , animationMgrEventConstructor :: EventM n s () -> e
                     , animationMgrNextAnimationID :: STM.TVar AnimationID
                     , animationMgrRunning :: STM.TVar Bool
                     }

-- NOTE: should figure out if this should be configurable and, if so,
-- whether it should be bounded in any way to avoid pitfalls.
tickMilliseconds :: Int
tickMilliseconds = 100

tickThreadBody :: STM.TChan (AnimationManagerRequest s)
               -> IO ()
tickThreadBody outChan =
    forever $ do
        threadDelay $ tickMilliseconds * 1000
        now <- getCurrentTime
        STM.atomically $ STM.writeTChan outChan $ Tick now

setNextFrameTime :: UTCTime -> Animation s -> Animation s
setNextFrameTime t a = a { animationNextFrameTime = t }

nominalDiffFromMs :: Integer -> NominalDiffTime
nominalDiffFromMs i = realToFrac (fromIntegral i / (100.0::Float))

animationManagerThreadBody :: STM.TChan (AnimationManagerRequest s)
                           -> BChan e
                           -> (EventM n s () -> e)
                           -> IO ()
animationManagerThreadBody inChan outChan mkEvent =
    let run = do
            req <- liftIO $ STM.atomically $ STM.readTChan inChan
            case req of
                StartAnimation a -> do
                    -- Schedule the animation, setting its next frame time.
                    now <- liftIO getCurrentTime
                    let next = addUTCTime frameOffset now
                        frameOffset = nominalDiffFromMs (animationFrameMilliseconds a)
                    modify $ HM.insert (animationID a) (setNextFrameTime next a)
                    run

                StopAnimation aId -> do
                    mA <- gets (HM.lookup aId)
                    case mA of
                        Nothing -> return ()
                        Just a -> do
                            -- Remove the animation from the manager
                            modify $ HM.delete aId

                            -- Set the current frame in the application
                            -- state to none
                            liftIO $ writeBChan outChan $
                                mkEvent $ do
                                    animationFrameUpdater a .= Nothing

                    run

                Tick tickTime -> do
                    -- Check all animation states for frame advances
                    -- based on the relationship between the tick time
                    -- and each animation's next frame time
                    advanced <- checkForFrames tickTime
                    when (not $ null advanced) $
                        liftIO $ writeBChan outChan $ mkEvent $ return ()

                    run

                Shutdown ->
                    return ()

    in evalStateT run mempty

checkForFrames :: UTCTime
               -> StateT (HM.HashMap AnimationID (Animation s)) IO [AnimationID]
checkForFrames _ = return []

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

startAnimationManager :: BChan e -> (EventM n s () -> e) -> IO (AnimationManager s e n)
startAnimationManager outChan mkEvent = do
    inChan <- STM.newTChanIO
    reqTid <- forkIO $ animationManagerThreadBody inChan outChan mkEvent
    tickTid <- forkIO $ tickThreadBody inChan
    runningVar <- STM.newTVarIO True
    idVar <- STM.newTVarIO $ AnimationID 1
    return $ AnimationManager { animationMgrRequestThreadId = reqTid
                              , animationMgrTickThreadId = tickTid
                              , animationMgrEventConstructor = mkEvent
                              , animationMgrOutputChan = outChan
                              , animationMgrInputChan = inChan
                              , animationMgrNextAnimationID = idVar
                              , animationMgrRunning = runningVar
                              }

whenRunning :: AnimationManager s e n -> IO () -> IO ()
whenRunning mgr act = do
    running <- STM.atomically $ STM.readTVar (animationMgrRunning mgr)
    when running act

stopAnimationManager :: AnimationManager s e n -> IO ()
stopAnimationManager mgr =
    whenRunning mgr $ do
        let reqTid = animationMgrRequestThreadId mgr
            tickTid = animationMgrTickThreadId mgr
        killThread reqTid
        killThread tickTid
        STM.atomically $ STM.writeTVar (animationMgrRunning mgr) False

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
