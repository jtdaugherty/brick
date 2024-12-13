{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Brick.Animation
  ( AnimationManager
  , Animation
  , animationFrameIndex
  , RunMode(..)
  , startAnimationManager
  , stopAnimationManager
  , startAnimation
  , stopAnimation
  , minTickTime
  , renderAnimation
  , FrameSeq
  , newFrameSeq
  , pingPongFrames
  , reverseFrames
  )
where

import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, when)
import Control.Monad.State.Strict
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Vector as V
import qualified Data.Time.Clock as C
import Lens.Micro ((^.), (%~), (.~), (&), Traversal', _Just)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl

import Brick.BChan
import Brick.Types (EventM, Widget)

newtype FrameSeq s n = FrameSeq (V.Vector (s -> Widget n))
                     deriving (Semigroup)

newFrameSeq :: [s -> Widget n] -> FrameSeq s n
newFrameSeq [] = error "newFrameSeq: got an empty list"
newFrameSeq fs = FrameSeq $ V.fromList fs

-- | Extend a frame sequence so that when the original sequence end is
-- reached, it reverses order.
--
-- For example, if this is given frames A, B, C, and D, then this
-- returns a frame sequence A, B, C, D, C, B.
--
-- If the given sequence contains less than two frames, this is
-- equivalent to 'id'.
pingPongFrames :: FrameSeq s n -> FrameSeq s n
pingPongFrames (FrameSeq fs) | V.length fs >= 2 =
    FrameSeq $ fs <> V.reverse (V.init $ V.tail fs)
pingPongFrames fs = fs

-- | Reverse a frame sequence.
reverseFrames :: FrameSeq s n -> FrameSeq s n
reverseFrames (FrameSeq fs) = FrameSeq $ V.reverse fs

data AnimationManagerRequest s n =
    Tick C.UTCTime
    | StartAnimation (FrameSeq s n) Integer RunMode (Traversal' s (Maybe (Animation s n)))
    -- ^ ID, frame count, frame duration in milliseconds, run mode, updater
    | StopAnimation (Animation s n)

data RunMode = Once | Loop
    deriving (Eq, Show, Ord)

newtype AnimationID = AnimationID Int
                    deriving (Eq, Ord, Show, Hashable)

data Animation s n =
    Animation { animationFrameIndex :: Int
              , animationID :: AnimationID
              , animationFrames :: FrameSeq s n
              }

renderAnimation :: Widget n -> s -> Maybe (Animation s n) -> Widget n
renderAnimation fallback input mAnim =
    draw input
    where
        draw = fromMaybe (const fallback) $ do
            a <- mAnim
            let idx = animationFrameIndex a
                FrameSeq fs = animationFrames a
            fs V.!? idx

data AnimationState s n =
    AnimationState { _animationStateID :: AnimationID
                   , _animationNumFrames :: Int
                   , _animationCurrentFrame :: Int
                   , _animationFrameMilliseconds :: Integer
                   , _animationRunMode :: RunMode
                   , animationFrameUpdater :: Traversal' s (Maybe (Animation s n))
                   , _animationNextFrameTime :: C.UTCTime
                   }

makeLenses ''AnimationState

data AnimationManager s e n =
    AnimationManager { animationMgrRequestThreadId :: ThreadId
                     , animationMgrTickThreadId :: ThreadId
                     , animationMgrOutputChan :: BChan e
                     , animationMgrInputChan :: STM.TChan (AnimationManagerRequest s n)
                     , animationMgrEventConstructor :: EventM n s () -> e
                     , animationMgrRunning :: STM.TVar Bool
                     }

tickThreadBody :: Int
               -> STM.TChan (AnimationManagerRequest s n)
               -> IO ()
tickThreadBody tickMilliseconds outChan =
    forever $ do
        threadDelay $ tickMilliseconds * 1000
        now <- C.getCurrentTime
        STM.atomically $ STM.writeTChan outChan $ Tick now

setNextFrameTime :: C.UTCTime -> AnimationState s n -> AnimationState s n
setNextFrameTime t a = a & animationNextFrameTime .~ t

nominalDiffFromMs :: Integer -> C.NominalDiffTime
nominalDiffFromMs i = realToFrac (fromIntegral i / (1000.0::Float))

nominalDiffToMs :: C.NominalDiffTime -> Integer
nominalDiffToMs t =
    -- NOTE: probably wrong, but we'll have to find out what this gives us
    (round $ C.nominalDiffTimeToSeconds t)

data ManagerState s e n =
    ManagerState { _managerStateInChan :: STM.TChan (AnimationManagerRequest s n)
                 , _managerStateOutChan :: BChan e
                 , _managerStateEventBuilder :: EventM n s () -> e
                 , _managerStateAnimations :: HM.HashMap AnimationID (AnimationState s n)
                 , _managerStateIDVar :: STM.TVar AnimationID
                 }

makeLenses ''ManagerState

animationManagerThreadBody :: STM.TChan (AnimationManagerRequest s n)
                           -> BChan e
                           -> (EventM n s () -> e)
                           -> IO ()
animationManagerThreadBody inChan outChan mkEvent = do
    idVar <- STM.newTVarIO $ AnimationID 1
    let initial = ManagerState { _managerStateInChan = inChan
                               , _managerStateOutChan = outChan
                               , _managerStateEventBuilder = mkEvent
                               , _managerStateAnimations = mempty
                               , _managerStateIDVar = idVar
                               }
    evalStateT runManager initial

type ManagerM s e n a = StateT (ManagerState s e n) IO a

getNextManagerRequest :: ManagerM s e n (AnimationManagerRequest s n)
getNextManagerRequest = do
    inChan <- use managerStateInChan
    liftIO $ STM.atomically $ STM.readTChan inChan

sendApplicationEvent :: EventM n s () -> ManagerM s e n ()
sendApplicationEvent act = do
    outChan <- use managerStateOutChan
    mkEvent <- use managerStateEventBuilder
    liftIO $ writeBChan outChan $ mkEvent act

removeAnimation :: AnimationID -> ManagerM s e n ()
removeAnimation aId =
    managerStateAnimations %= HM.delete aId

lookupAnimation :: AnimationID -> ManagerM s e n (Maybe (AnimationState s n))
lookupAnimation aId =
    HM.lookup aId <$> use managerStateAnimations

insertAnimation :: AnimationState s n -> ManagerM s e n ()
insertAnimation a =
    managerStateAnimations %= HM.insert (a^.animationStateID) a

getNextAnimationID :: ManagerM s e n AnimationID
getNextAnimationID = do
    var <- use managerStateIDVar
    liftIO $ STM.atomically $ do
        AnimationID i <- STM.readTVar var
        let next = AnimationID $ i + 1
        STM.writeTVar var next
        return $ AnimationID i

runManager :: ManagerM s e n ()
runManager = forever $ do
    getNextManagerRequest >>= handleManagerRequest

handleManagerRequest :: AnimationManagerRequest s n -> ManagerM s e n ()
handleManagerRequest (StartAnimation frames@(FrameSeq fs) frameMs runMode updater) = do
    aId <- getNextAnimationID
    now <- liftIO C.getCurrentTime
    let next = C.addUTCTime frameOffset now
        frameOffset = nominalDiffFromMs frameMs
        a = AnimationState { _animationStateID = aId
                           , _animationNumFrames = V.length fs
                           , _animationCurrentFrame = 0
                           , _animationFrameMilliseconds = frameMs
                           , _animationRunMode = runMode
                           , animationFrameUpdater = updater
                           , _animationNextFrameTime = next
                           }

    insertAnimation a
    sendApplicationEvent $ updater .= Just (Animation { animationID = aId
                                                      , animationFrameIndex = 0
                                                      , animationFrames = frames
                                                      })
handleManagerRequest (StopAnimation a) = do
    let aId = animationID a
    mA <- lookupAnimation aId
    case mA of
        Nothing -> return ()
        Just aState -> do
            -- Remove the animation from the manager
            removeAnimation aId

            -- Set the current animation state in the application state
            -- to none
            sendApplicationEvent $ clearStateAction aState
handleManagerRequest (Tick tickTime) = do
    -- Check all animation states for frame advances
    -- based on the relationship between the tick time
    -- and each animation's next frame time
    mUpdateAct <- checkForFrames tickTime
    case mUpdateAct of
        Nothing -> return ()
        Just act -> sendApplicationEvent act

clearStateAction :: AnimationState s n -> EventM n s ()
clearStateAction a = animationFrameUpdater a .= Nothing

frameUpdateAction :: AnimationState s n -> EventM n s ()
frameUpdateAction a =
    animationFrameUpdater a._Just %=
        (\an -> an { animationFrameIndex = a^.animationCurrentFrame })

updateAnimationState :: C.UTCTime -> AnimationState s n -> AnimationState s n
updateAnimationState now a =
    let differenceMs = nominalDiffToMs $
                       C.diffUTCTime now (a^.animationNextFrameTime)
        numFrames = 1 + (differenceMs `div` (a^.animationFrameMilliseconds))
        newNextTime = C.addUTCTime (nominalDiffFromMs $ numFrames * (a^.animationFrameMilliseconds))
                                   (a^.animationNextFrameTime)

    -- The new frame is obtained by advancing from the current frame by
    -- numFrames.
    in setNextFrameTime newNextTime $ advanceBy numFrames a

checkForFrames :: C.UTCTime -> ManagerM s e n (Maybe (EventM n s ()))
checkForFrames now = do
    -- For each active animation, check to see if the animation's next
    -- frame time has passed. If it has, advance its frame counter as
    -- appropriate and schedule its frame counter to be updated in the
    -- application state.
    let getUpdater :: AnimationState s n -> ManagerM s e n (Maybe (EventM n s ()))
        getUpdater a
            | (now < a^.animationNextFrameTime) =
                -- This animation is not due for an
                -- update, so don't do anything.
                return Nothing
            | isFinished a = do
                -- This animation has completed, so
                -- clear it from the manager and the
                -- application state.
                removeAnimation (a^.animationStateID)
                return $ Just $ clearStateAction a
            | otherwise = do
                -- This animation is still running,
                -- so determine how many frames have
                -- elapsed for it and then advance the
                -- frame index based the elapsed time.
                -- Also set its next frame time.
                let a' = updateAnimationState now a
                managerStateAnimations %= HM.insert (a'^.animationStateID) a'
                return $ Just $ frameUpdateAction a'

    as <- HM.elems <$> use managerStateAnimations
    updaters <- catMaybes <$> mapM getUpdater as
    case updaters of
        [] -> return Nothing
        _ -> return $ Just $ sequence_ updaters

isFinished :: AnimationState s n -> Bool
isFinished a =
    a^.animationRunMode == Once &&
    a^.animationCurrentFrame == a^.animationNumFrames - 1

advanceBy :: Integer -> AnimationState s n -> AnimationState s n
advanceBy n a
    | n <= 0 = a
    | otherwise =
        advanceBy (n - 1) $
        advanceByOne a

advanceByOne :: AnimationState s n -> AnimationState s n
advanceByOne a =
    if a^.animationCurrentFrame == a^.animationNumFrames - 1
    then case a^.animationRunMode of
        Loop -> a & animationCurrentFrame .~ 0
        Once -> a
    else a & animationCurrentFrame %~ (+ 1)

minTickTime :: Int
minTickTime = 25

startAnimationManager :: Int -> BChan e -> (EventM n s () -> e) -> IO (AnimationManager s e n)
startAnimationManager tickMilliseconds _ _ | tickMilliseconds < minTickTime =
    error $ "startAnimationManager: tick delay too small (minimum is " <> show minTickTime <> ")"
startAnimationManager tickMilliseconds outChan mkEvent = do
    inChan <- STM.newTChanIO
    reqTid <- forkIO $ animationManagerThreadBody inChan outChan mkEvent
    tickTid <- forkIO $ tickThreadBody tickMilliseconds inChan
    runningVar <- STM.newTVarIO True
    return $ AnimationManager { animationMgrRequestThreadId = reqTid
                              , animationMgrTickThreadId = tickTid
                              , animationMgrEventConstructor = mkEvent
                              , animationMgrOutputChan = outChan
                              , animationMgrInputChan = inChan
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

tellAnimationManager :: (MonadIO m)
                     => AnimationManager s e n -> AnimationManagerRequest s n -> m ()
tellAnimationManager mgr req =
    liftIO $
    STM.atomically $
    STM.writeTChan (animationMgrInputChan mgr) req

startAnimation :: (MonadIO m)
               => AnimationManager s e n
               -> FrameSeq s n
               -> Integer
               -> RunMode
               -> Traversal' s (Maybe (Animation s n))
               -> m ()
startAnimation mgr frames frameMs runMode updater =
    tellAnimationManager mgr $ StartAnimation frames frameMs runMode updater

stopAnimation :: (MonadIO m)
              => AnimationManager s e n
              -> Animation s n
              -> m ()
stopAnimation mgr a =
    tellAnimationManager mgr $ StopAnimation a
