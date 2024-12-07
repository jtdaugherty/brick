{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Brick.Animation
  ( AnimationManager
  , Animation
  , animationFrameIndex
  , Duration(..)
  , AnimationMode(..)
  , startAnimationManager
  , stopAnimationManager
  , startAnimation
  , stopAnimation
  , renderAnimation
  , Frames
  , newFrames
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
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Data.Time.Clock
import Lens.Micro ((^.), (%~), (.~), (&), Traversal', _Just)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl

import Brick.BChan
import Brick.Types (EventM, Widget)

newtype Frames s n = Frames (V.Vector (s -> Widget n))

newFrames :: [s -> Widget n] -> Frames s n
newFrames = Frames . V.fromList

-- | Given a frame sequence, extend it so that when the original
-- sequence end is reached, it reverses order.
--
-- For example, if this is given frames A, B, C, and D, then this
-- returns a frame sequence A, B, C, D, C, B.
--
-- If the given 'Frames' contains less than two frames, this is
-- equivalent to 'id'.
pingPongFrames :: Frames s n -> Frames s n
pingPongFrames (Frames fs) | V.length fs >= 2 =
    Frames $ fs <> V.reverse (V.init $ V.tail fs)
pingPongFrames fs = fs

reverseFrames :: Frames s n -> Frames s n
reverseFrames (Frames fs) = Frames $ V.reverse fs

data AnimationManagerRequest s n =
    Tick UTCTime
    | StartAnimation (Frames s n) Integer AnimationMode Duration (Traversal' s (Maybe (Animation s n)))
    -- ^ ID, frame count, frame duration in milliseconds, mode, duration, updater
    | StopAnimation (Animation s n)

-- Is this a good name for this type? If we added a 'manual' option
-- where the application does frame updates, would it go here?
data Duration = Once | Loop
    deriving (Eq, Show, Ord)

data AnimationMode =
    Forward
    -- | Random
    deriving (Eq, Show, Ord)

newtype AnimationID = AnimationID Int
                    deriving (Eq, Ord, Show, Hashable)

data Animation s n =
    Animation { animationFrameIndex :: Int
              , animationID :: AnimationID
              , animationFrames :: Frames s n
              }

renderAnimation :: Widget n -> s -> Maybe (Animation s n) -> Widget n
renderAnimation fallback input mAnim =
    draw input
    where
        draw = fromMaybe (const fallback) $ do
            a <- mAnim
            let idx = animationFrameIndex a
                Frames fs = animationFrames a
            fs V.!? idx

data AnimationState s n =
    AnimationState { _animationStateID :: AnimationID
                   , _animationNumFrames :: Int
                   , _animationCurrentFrame :: Int
                   , _animationPreviousFrame :: Maybe Int
                   , _animationFrameMilliseconds :: Integer
                   -- what about tracking that an animation is currently
                   -- moving backward when it sometimes moves forward? Just
                   -- track the previous frame always, and use that? that
                   -- works in general (can be ignored in the random case but
                   -- is used in all others)
                   , _animationMode :: AnimationMode
                   , _animationDuration :: Duration
                   , animationFrameUpdater :: Traversal' s (Maybe (Animation s n))
                   , _animationNextFrameTime :: UTCTime
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

-- NOTE: should figure out if this should be configurable and, if so,
-- whether it should be bounded in any way to avoid pitfalls.
tickMilliseconds :: Int
tickMilliseconds = 100

tickThreadBody :: STM.TChan (AnimationManagerRequest s n)
               -> IO ()
tickThreadBody outChan =
    forever $ do
        threadDelay $ tickMilliseconds * 1000
        now <- getCurrentTime
        STM.atomically $ STM.writeTChan outChan $ Tick now

setNextFrameTime :: UTCTime -> AnimationState s n -> AnimationState s n
setNextFrameTime t a = a & animationNextFrameTime .~ t

nominalDiffFromMs :: Integer -> NominalDiffTime
nominalDiffFromMs i = realToFrac (fromIntegral i / (1000.0::Float))

nominalDiffToMs :: NominalDiffTime -> Integer
nominalDiffToMs t =
    -- NOTE: probably wrong, but we'll have to find out what this gives us
    (round $ nominalDiffTimeToSeconds t)

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
handleManagerRequest (StartAnimation frames@(Frames fs) frameMs mode dur updater) = do
    aId <- getNextAnimationID
    now <- liftIO getCurrentTime
    let next = addUTCTime frameOffset now
        frameOffset = nominalDiffFromMs frameMs
        a = AnimationState { _animationStateID = aId
                           , _animationNumFrames = V.length fs
                           , _animationCurrentFrame = 0
                           , _animationPreviousFrame = Nothing
                           , _animationFrameMilliseconds = frameMs
                           , _animationMode = mode
                           , _animationDuration = dur
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
            sendApplicationEvent $ do
                animationFrameUpdater aState .= Nothing
handleManagerRequest (Tick tickTime) = do
    -- Check all animation states for frame advances
    -- based on the relationship between the tick time
    -- and each animation's next frame time
    mUpdateAct <- checkForFrames tickTime
    case mUpdateAct of
        Nothing -> return ()
        Just act -> sendApplicationEvent act

checkForFrames :: UTCTime -> ManagerM s e n (Maybe (EventM n s ()))
checkForFrames now = do
    -- For each active animation, check to see if the animation's next
    -- frame time has passed. If it has, advance its frame counter as
    -- appropriate and schedule its frame counter to be updated in the
    -- application state.
    let addUpdate a Nothing = Just $ updateFor a
        addUpdate a (Just updater) = Just $ updater >> updateFor a

        updateFor a = animationFrameUpdater a._Just %= (\an -> an { animationFrameIndex = a^.animationCurrentFrame })

        go :: Maybe (EventM n s ()) -> [AnimationState s n] -> ManagerM s e n (Maybe (EventM n s ()))
        go mUpdater [] = return mUpdater
        go mUpdater (a:as) = do
            -- Determine whether the next animation needs to have its
            -- frame index advanced.
            newUpdater <- if now < a^.animationNextFrameTime
                          then return mUpdater
                          else do
                              -- Determine how many frames have elapsed
                              -- for this animation, then advance the
                              -- frame index based the elapsed time.
                              -- Also set its next frame time.
                              let differenceMs = nominalDiffToMs $
                                                 diffUTCTime now (a^.animationNextFrameTime)
                                  numFrames = 1 + (differenceMs `div` (a^.animationFrameMilliseconds))
                                  newNextTime = addUTCTime (nominalDiffFromMs $ numFrames * (a^.animationFrameMilliseconds))
                                                           (a^.animationNextFrameTime)

                                  -- The new frame is obtained by
                                  -- advancing from the current frame by
                                  -- numFrames.
                                  a' = setNextFrameTime newNextTime $
                                       advanceBy numFrames a

                              managerStateAnimations %= HM.insert (a'^.animationStateID) a'

                              -- NOTE!
                              --
                              --
                              -- This always advances each animation
                              -- without regard for the loop mode. This
                              -- needs to be updated to account for the
                              -- Once mode where an animation reaches
                              -- its last frame and stays there.
                              --
                              -- A related question: if something
                              -- animates once, should it terminate by
                              -- staying in its last frame? Or should it
                              -- be unscheduled?
                              return $ addUpdate a' mUpdater
            go newUpdater as

    as <- HM.elems <$> use managerStateAnimations
    go Nothing as

advanceBy :: Integer -> AnimationState s n -> AnimationState s n
advanceBy n a
    | n <= 0 = a
    | otherwise =
        advanceBy (n - 1) $
        advanceByOne a

advanceByOne :: AnimationState s n -> AnimationState s n
advanceByOne a =
    case a^.animationMode of
        Forward ->
            if a^.animationCurrentFrame == a^.animationNumFrames - 1
            then case a^.animationDuration of
                Loop -> a & animationCurrentFrame .~ 0
                Once -> a
            else a & animationCurrentFrame %~ (+ 1)

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
               -> Frames s n
               -> Integer
               -> AnimationMode
               -> Duration
               -> Traversal' s (Maybe (Animation s n))
               -> m ()
startAnimation mgr frames frameMs mode duration updater = do
    tellAnimationManager mgr $ StartAnimation frames frameMs mode duration updater

stopAnimation :: (MonadIO m)
              => AnimationManager s e n
              -> Animation s n
              -> m ()
stopAnimation mgr a =
    tellAnimationManager mgr $ StopAnimation a
