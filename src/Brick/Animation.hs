{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
-- | This module provides some infrastructure for adding animations to
-- Brick applications. See @programs/AnimationDemo.hs@ for a complete
-- working example of this API.
--
-- At a high level, this works as follows:
--
-- This module provides a threaded animation manager that manages a set
-- of running animations. The application creates the manager and starts
-- animations, which automatically loop or run once, depending on their
-- configuration. Each animation has some state in the application's
-- state that is automatically managed by the animation manager using a
-- lens-based API. Whenever animations need to be redrawn, the animation
-- manager sends a custom event with a state update to the application,
-- which must be evaluated by the main event loop to update animation
-- states. Each animation is associated with a 'Clip' -- sequence of
-- frames -- which may be static or may be built from the application
-- state at rendering time.
--
-- To use this module:
--
-- * Use a custom event type @e@ in your 'Brick.Main.App' and give the
--   event type a constructor @EventM n s () -> e@ (where @s@ and
--   @n@ are those in @App s e n@). This will require the use of
--   'Brick.Main.customMain' and will also require the creation of a
--   'Brick.BChan.BChan' for custom events.
--
-- * Add an 'AnimationManager' field to the application state @s@.
--
-- * Create an 'AnimationManager' at startup with
--   'startAnimationManager', providing the custom event constructor and
--   'BChan' created above. Store the manager in the application state.
--
-- * For each animation you want to run at any given time, add a field
--   to the application state of type @Maybe (Animation s n)@,
--   initialized to 'Nothing'. A value of 'Nothing' indicates that the
--   animation is not running.
--
-- * Ensure that each animation state field in @s@ has a lens, usually
--   by using 'Lens.Micro.TH.makeLenses'.
--
-- * Start new animations in 'EventM' with 'startAnimation'; stop them
--   with 'stopAnimation'. Supply clips for new animations with
--   'newClip', 'newClip_', and the clip transformation functions.
--
-- * Call 'renderAnimation' in 'Brick.Main.appDraw' for each animation in the
--   application state.
--
-- * If needed, stop the animation manager with 'stopAnimationManager'.
--
-- See 'AnimationManager' and the docs for the rest of this module for
-- details.
module Brick.Animation
  ( -- * Animation managers
    AnimationManager
  , startAnimationManager
  , stopAnimationManager
  , minTickTime

  -- * Animations
  , Animation
  , animationFrameIndex

  -- * Starting and stopping animations
  , RunMode(..)
  , startAnimation
  , stopAnimation

  -- * Rendering animations
  , renderAnimation

  -- * Creating clips
  , Clip
  , newClip
  , newClip_
  , clipLength

  -- * Transforming clips
  , pingPongClip
  , reverseClip
  )
where

import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread, myThreadId)
import qualified Control.Concurrent.STM as STM
import Control.Monad (forever, when)
import Control.Monad.State.Strict
import Data.Foldable (foldrM)
import Data.Hashable (Hashable)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Lens.Micro ((^.), (%~), (.~), (&), Traversal', _Just)
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl

import Brick.BChan
import Brick.Types (EventM, Widget)
import qualified Brick.Animation.Clock as C

-- | A sequence of a animation frames.
newtype Clip s n = Clip (V.Vector (s -> Widget n))
                     deriving (Semigroup)

-- | Get the number of frames in a clip.
clipLength :: Clip s n -> Int
clipLength (Clip fs) = V.length fs

-- | Build a clip.
--
-- Each frame in a clip is represented by a function from a state to a
-- 'Widget'. This allows applications to determine on a per-frame basis
-- what should be drawn in an animation based on application state, if
-- desired, in the same style as 'Brick.Main.appDraw'.
--
-- If the provided list is empty, this calls 'error'.
newClip :: [s -> Widget n] -> Clip s n
newClip [] = error "clip: got an empty list"
newClip fs = Clip $ V.fromList fs

-- | Like 'newClip' but for static frames.
newClip_ :: [Widget n] -> Clip s n
newClip_ ws = newClip $ const <$> ws

-- | Extend a clip so that when the end of the original clip is reached,
-- it continues in reverse order to create a loop.
--
-- For example, if this is given a clip with frames A, B, C, and D, then
-- this returns a clip with frames A, B, C, D, C, and B.
--
-- If the given clip contains less than two frames, this is equivalent
-- to 'id'.
pingPongClip :: Clip s n -> Clip s n
pingPongClip (Clip fs) | V.length fs >= 2 =
    Clip $ fs <> V.reverse (V.init $ V.tail fs)
pingPongClip c = c

-- | Reverse a clip.
reverseClip :: Clip s n -> Clip s n
reverseClip (Clip fs) = Clip $ V.reverse fs

data AnimationManagerRequest s n =
    Tick C.Time
    | StartAnimation (Clip s n) Integer RunMode (Traversal' s (Maybe (Animation s n)))
    -- ^ Clip, frame duration in milliseconds, run mode, updater
    | StopAnimation (Animation s n)
    | Shutdown

-- | The running mode for an animation.
data RunMode =
    Once
    -- ^ Run the animation once and then end
    | Loop
    -- ^ Run the animation in a loop forever
    deriving (Eq, Show, Ord)

newtype AnimationID = AnimationID Int
                    deriving (Eq, Ord, Show, Hashable)

-- | The state of a running animation.
--
-- Put one of these (wrapped in 'Maybe') in your application state for
-- each animation that you'd like to run concurrently.
data Animation s n =
    Animation { animationFrameIndex :: Int
              -- ^ The animation's current frame index, provided for
              -- convenience. Applications won't need to access this in
              -- most situations; use 'renderAnimation' instead.
              , animationID :: AnimationID
              -- ^ The animation's internally-managed ID
              , animationClip :: Clip s n
              -- ^ The animation's clip
              }

-- | Render an animation.
renderAnimation :: (s -> Widget n)
                -- ^ The fallback function to use for drawing if the
                -- animation is not running
                -> s
                -- ^ The state to provide when rendering the animation's
                -- current frame
                -> Maybe (Animation s n)
                -- ^ The animation state itself
                -> Widget n
renderAnimation fallback input mAnim =
    draw input
    where
        draw = fromMaybe fallback $ do
            a <- mAnim
            let idx = animationFrameIndex a
                Clip fs = animationClip a
            fs V.!? idx

data AnimationState s n =
    AnimationState { _animationStateID :: AnimationID
                   , _animationNumFrames :: Int
                   , _animationCurrentFrame :: Int
                   , _animationFrameMilliseconds :: Integer
                   , _animationRunMode :: RunMode
                   , animationFrameUpdater :: Traversal' s (Maybe (Animation s n))
                   , _animationNextFrameTime :: C.Time
                   }

makeLenses ''AnimationState

-- | A manager for animations. The type variables for this type are the
-- same as those for 'Brick.Main.App'.
--
-- This asynchronously manages a set of running animations, advancing
-- each one over time. When a running animation's current frame needs
-- to be changed, the manager sends an 'EventM' update for that
-- animation to the application's event loop to perform the update to
-- the animation in the application state. The manager will batch such
-- updates if more than one animation needs to be changed at a time.
--
-- The manager has a /tick duration/ in milliseconds which is the
-- resolution at which animations are checked to see if they should
-- be updated. Animations also have their own frame duration in
-- milliseconds. For example, if a manager has a tick duration of 50
-- milliseconds and is running an animation with a frame duration of 100
-- milliseconds, then the manager will advance that animation by one
-- frame every two ticks. On the other hand, if a manager has a tick
-- duration of 100 milliseconds and is running an animation with a frame
-- duration of 50 milliseconds, the manager will advance that animation
-- by two frames on each tick.
--
-- Animation managers are started with 'startAnimationManager' and
-- stopped with 'stopAnimationManager'.
--
-- Animations are started with 'startAnimation' and stopped with
-- 'stopAnimation'. Each animation must be associated with an
-- application state field accessible with a traversal given to
-- 'startAnimation'.
--
-- When an animation is started, every time it advances a frame, and
-- when it is ended, the manager communicates these changes to the
-- application by using the custom event constructor provided to
-- 'startAnimationManager'. The manager uses that to schedule a state
-- update which the application is responsible for evaluating. The state
-- updates are built from the traversals provided to 'startAnimation'.
--
-- The manager-updated 'Animation' values in the application state are
-- then drawn with 'renderAnimation'.
--
-- Animations in 'Loop' mode are run forever until stopped with
-- 'stopAnimation'; animations in 'Once' mode run once and are removed
-- from the application state (set to 'Nothing') when they finish. All
-- state updates to the application state are performed by the manager's
-- custom event mechanism; the application never needs to directly
-- modify the 'Animation' application state fields except to initialize
-- them to 'Nothing'.
--
-- There is nothing here to prevent an application from running multiple
-- managers, each at a different tick rate. That may have performance
-- consequences, though, due to the loss of batch efficiency in state
-- updates, so we recommend using only one manager per application at a
-- sufficiently short tick duration.
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
tickThreadBody tickMilliseconds outChan = do
    let nextTick = C.addOffset tickOffset
        tickOffset = C.offsetFromMs $ toInteger tickMilliseconds
        go targetTime = do
            now <- C.getTime
            STM.atomically $ STM.writeTChan outChan $ Tick now

            -- threadDelay does not guarantee that we will wake up on
            -- time; it only ensures that we won't wake up earlier than
            -- requested. Since we can therefore oversleep, instead of
            -- always sleeping for tickMilliseconds (which would cause
            -- us to drift off of schedule as delays accumulate) we
            -- determine sleep time by measuring the distance between
            -- now and the next scheduled tick. This is still unreliable
            -- as we can still oversleep, but it keeps the oversleeping
            -- under control over time. It means most ticks may be
            -- slightly late (about 1-2 milliseconds is common) but this
            -- will prevent that per-tick error from accumulating.
            let nextTickTime = nextTick targetTime
                sleepMs = fromInteger $
                          C.offsetToMs $
                          C.subtractTime nextTickTime now

            -- threadDelay works microseconds.
            threadDelay $ sleepMs * 1000
            go nextTickTime

    go =<< C.getTime

setNextFrameTime :: C.Time -> AnimationState s n -> AnimationState s n
setNextFrameTime t a = a & animationNextFrameTime .~ t

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

sendApplicationStateUpdate :: EventM n s () -> ManagerM s e n ()
sendApplicationStateUpdate act = do
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
handleManagerRequest (StartAnimation clip frameMs runMode updater) = do
    aId <- getNextAnimationID
    now <- liftIO C.getTime
    let next = C.addOffset frameOffset now
        frameOffset = C.offsetFromMs frameMs
        a = AnimationState { _animationStateID = aId
                           , _animationNumFrames = clipLength clip
                           , _animationCurrentFrame = 0
                           , _animationFrameMilliseconds = frameMs
                           , _animationRunMode = runMode
                           , animationFrameUpdater = updater
                           , _animationNextFrameTime = next
                           }

    insertAnimation a
    sendApplicationStateUpdate $ updater .= Just (Animation { animationID = aId
                                                            , animationFrameIndex = 0
                                                            , animationClip = clip
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
            sendApplicationStateUpdate $ clearStateAction aState
handleManagerRequest Shutdown = do
    as <- HM.elems <$> use managerStateAnimations

    let updater = sequence_ $ clearStateAction <$> as
    when (not $ null as) $ do
        sendApplicationStateUpdate updater

    liftIO $ myThreadId >>= killThread
handleManagerRequest (Tick tickTime) = do
    -- Check all animation states for frame advances
    -- based on the relationship between the tick time
    -- and each animation's next frame time
    mUpdateAct <- checkAnimations tickTime
    case mUpdateAct of
        Nothing -> return ()
        Just act -> sendApplicationStateUpdate act

clearStateAction :: AnimationState s n -> EventM n s ()
clearStateAction a = animationFrameUpdater a .= Nothing

frameUpdateAction :: AnimationState s n -> EventM n s ()
frameUpdateAction a =
    animationFrameUpdater a._Just %=
        (\an -> an { animationFrameIndex = a^.animationCurrentFrame })

updateAnimationState :: C.Time -> AnimationState s n -> AnimationState s n
updateAnimationState now a =
    let differenceMs = C.offsetToMs $
                       C.subtractTime now (a^.animationNextFrameTime)
        numFrames = 1 + (differenceMs `div` (a^.animationFrameMilliseconds))
        newNextTime = C.addOffset (C.offsetFromMs $ numFrames * (a^.animationFrameMilliseconds))
                                  (a^.animationNextFrameTime)

    -- The new frame is obtained by advancing from the current frame by
    -- numFrames.
    in setNextFrameTime newNextTime $ advanceBy numFrames a

checkAnimations :: C.Time -> ManagerM s e n (Maybe (EventM n s ()))
checkAnimations now = do
    let go a updaters = do
          result <- checkAnimation now a
          return $ case result of
              Nothing -> updaters
              Just u  -> u : updaters

    anims <- use managerStateAnimations
    updaters <- foldrM go [] anims

    case updaters of
        [] -> return Nothing
        _ -> return $ Just $ sequence_ updaters

-- For each active animation, check to see if the animation's next frame
-- time has passed. If it has, advance its frame counter as appropriate
-- and schedule its frame index to be updated in the application state.
checkAnimation :: C.Time -> AnimationState s n -> ManagerM s e n (Maybe (EventM n s ()))
checkAnimation now a
    | isFinished a = do
        -- This animation completed in a previous check, so clear it
        -- from the manager and the application state.
        removeAnimation (a^.animationStateID)
        return $ Just $ clearStateAction a
    | (now < a^.animationNextFrameTime) =
        -- This animation is not due for an update, so don't do
        -- anything.
        return Nothing
    | otherwise = do
        -- This animation is still running, so determine how many frames
        -- have elapsed for it and then advance the frame index based
        -- the elapsed time. Also set its next frame time.
        let a' = updateAnimationState now a
        managerStateAnimations %= HM.insert (a'^.animationStateID) a'
        return $ Just $ frameUpdateAction a'

isFinished :: AnimationState s n -> Bool
isFinished a =
    case a^.animationRunMode of
        Once -> a^.animationCurrentFrame == a^.animationNumFrames - 1
        Loop -> False

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

-- | The minimum tick duration in milliseconds allowed by
-- 'startAnimationManager'.
minTickTime :: Int
minTickTime = 25

-- | Start a new animation manager. For full details about how managers
-- work, see 'AnimationManager'.
--
-- If the specified tick duration is less than 'minTickTime', this will
-- call 'error'. This bound is in place to prevent API misuse leading to
-- ticking so fast that the terminal can't keep up with redraws.
startAnimationManager :: (MonadIO m)
                      => Int
                      -- ^ The tick duration for this manager in milliseconds
                      -> BChan e
                      -- ^ The event channel to use to send updates to
                      -- the application (i.e. the same one given to
                      -- e.g. 'Brick.Main.customVty')
                      -> (EventM n s () -> e)
                      -- ^ A constructor for building custom events
                      -- that perform application state updates. The
                      -- application must evaluate these custom events'
                      -- 'EventM' actions in order to record animation
                      -- updates in the application state.
                      -> m (AnimationManager s e n)
startAnimationManager tickMilliseconds _ _ | tickMilliseconds < minTickTime =
    error $ "startAnimationManager: tick duration too small (minimum is " <> show minTickTime <> ")"
startAnimationManager tickMilliseconds outChan mkEvent = liftIO $ do
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

-- | Execute the specified action only when this manager is running.
whenRunning :: (MonadIO m) => AnimationManager s e n -> IO () -> m ()
whenRunning mgr act = do
    running <- liftIO $ STM.atomically $ STM.readTVar (animationMgrRunning mgr)
    when running $ liftIO act

-- | Stop the animation manager, ending all running animations.
stopAnimationManager :: (MonadIO m) => AnimationManager s e n -> m ()
stopAnimationManager mgr =
    whenRunning mgr $ do
        tellAnimationManager mgr Shutdown
        killThread $ animationMgrTickThreadId mgr
        STM.atomically $ STM.writeTVar (animationMgrRunning mgr) False

-- | Send a request to an animation manager.
tellAnimationManager :: (MonadIO m)
                     => AnimationManager s e n
                     -- ^ The manager
                     -> AnimationManagerRequest s n
                     -- ^ The request to send
                     -> m ()
tellAnimationManager mgr req =
    liftIO $
    STM.atomically $
    STM.writeTChan (animationMgrInputChan mgr) req

-- | Start a new animation at its first frame.
--
-- This will result in an application state update to initialize the
-- animation state at the provided traversal's location.
startAnimation :: (MonadIO m)
               => AnimationManager s e n
               -- ^ The manager to run the animation
               -> Clip s n
               -- ^ The frames for the animation
               -> Integer
               -- ^ The animation's frame duration in milliseconds
               -> RunMode
               -- ^ The animation's run mode
               -> Traversal' s (Maybe (Animation s n))
               -- ^ Where in the application state to manage this
               -- animation's state
               -> m ()
startAnimation mgr frames frameMs runMode updater =
    tellAnimationManager mgr $ StartAnimation frames frameMs runMode updater

-- | Stop an animation.
--
-- This will result in an application state update to remove the
-- animation state.
stopAnimation :: (MonadIO m)
              => AnimationManager s e n
              -> Animation s n
              -> m ()
stopAnimation mgr a =
    tellAnimationManager mgr $ StopAnimation a
