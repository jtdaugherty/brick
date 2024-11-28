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
import Data.Time.Clock
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
    | StartAnimation Int Integer AnimationMode Duration (Traversal' s (Maybe Int))
    -- ^ Frame count, frame duration in milliseconds, mode, duration, updater
    | StopAnimation AnimationID

-- Is this a good name for this type? If we added a 'manual' option
-- where the application does frame updates, would it go here?
data Duration = Once | Loop
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

nominalDiffToMs :: NominalDiffTime -> Integer
nominalDiffToMs t =
    -- NOTE: probably wrong, but we'll have to find out what this gives us
    (round $ nominalDiffTimeToSeconds t)

data ManagerState s e n =
    ManagerState { managerStateInChan :: STM.TChan (AnimationManagerRequest s)
                 , managerStateOutChan :: BChan e
                 , managerStateEventBuilder :: EventM n s () -> e
                 , managerStateAnimations :: HM.HashMap AnimationID (Animation s)
                 , managerStateIDVar :: STM.TVar AnimationID
                 }

animationManagerThreadBody :: STM.TChan (AnimationManagerRequest s)
                           -> BChan e
                           -> (EventM n s () -> e)
                           -> STM.TVar AnimationID
                           -> IO ()
animationManagerThreadBody inChan outChan mkEvent idVar =
    let initial = ManagerState { managerStateInChan = inChan
                               , managerStateOutChan = outChan
                               , managerStateEventBuilder = mkEvent
                               , managerStateAnimations = mempty
                               , managerStateIDVar = idVar
                               }
    in evalStateT runManager initial

type ManagerM s e n a = StateT (ManagerState s e n) IO a

getNextManagerRequest :: ManagerM s e n (AnimationManagerRequest s)
getNextManagerRequest = do
    inChan <- gets managerStateInChan
    liftIO $ STM.atomically $ STM.readTChan inChan

sendApplicationEvent :: EventM n s () -> ManagerM s e n ()
sendApplicationEvent act = do
    outChan <- gets managerStateOutChan
    mkEvent <- gets managerStateEventBuilder
    liftIO $ writeBChan outChan $ mkEvent act

removeAnimation :: AnimationID -> ManagerM s e n ()
removeAnimation aId =
    modify $ \s ->
        s { managerStateAnimations = HM.delete aId (managerStateAnimations s) }

lookupAnimation :: AnimationID -> ManagerM s e n (Maybe (Animation s))
lookupAnimation aId =
    gets (HM.lookup aId . managerStateAnimations)

insertAnimation :: Animation s -> ManagerM s e n ()
insertAnimation a =
    modify $ \s ->
        s { managerStateAnimations = HM.insert (animationID a) a (managerStateAnimations s) }

getNextAnimationID :: ManagerM s e n AnimationID
getNextAnimationID = do
    var <- gets managerStateIDVar
    liftIO $ STM.atomically $ do
        AnimationID i <- STM.readTVar var
        let next = AnimationID $ i + 1
        STM.writeTVar var next
        return next

runManager :: ManagerM s e n ()
runManager = forever $ do
    req <- getNextManagerRequest
    case req of
        StartAnimation numFrames frameMs mode dur updater -> do
            aId <- getNextAnimationID

            now <- liftIO getCurrentTime
            let next = addUTCTime frameOffset now
                frameOffset = nominalDiffFromMs (animationFrameMilliseconds a)
                a = Animation { animationID = aId
                              , animationNumFrames = numFrames
                              , animationCurrentFrame = 0
                              , animationPreviousFrame = Nothing
                              , animationFrameMilliseconds = frameMs
                              , animationMode = mode
                              , animationDuration = dur
                              , animationFrameUpdater = updater
                              , animationNextFrameTime = next
                              }

            insertAnimation a

        StopAnimation aId -> do
            mA <- lookupAnimation aId
            case mA of
                Nothing -> return ()
                Just a -> do
                    -- Remove the animation from the manager
                    removeAnimation aId

                    -- Set the current frame in the application
                    -- state to none
                    sendApplicationEvent $ do
                        animationFrameUpdater a .= Nothing

        Tick tickTime -> do
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

        updateFor a = animationFrameUpdater a .= Just (animationCurrentFrame a)

        go :: Maybe (EventM n s ()) -> [Animation s] -> ManagerM s e n (Maybe (EventM n s ()))
        go mUpdater [] = return mUpdater
        go mUpdater (a:as) = do
            -- Determine whether the next animation needs to have its
            -- frame index advanced.
            newUpdater <- if now < animationNextFrameTime a
                          then return mUpdater
                          else do
                              -- Determine how many frames have elapsed
                              -- for this animation, then advance the
                              -- frame index based the elapsed time.
                              -- Also set its next frame time.
                              let differenceMs = nominalDiffToMs $
                                                 diffUTCTime now (animationNextFrameTime a)
                                  numFrames = 1 + (differenceMs `div` animationFrameMilliseconds a)
                                  newNextTime = addUTCTime (nominalDiffFromMs $ numFrames * (animationFrameMilliseconds a))
                                                           (animationNextFrameTime a)

                                  -- The new frame is obtained by
                                  -- advancing from the current frame by
                                  -- numFrames.
                                  a' = setNextFrameTime newNextTime $
                                       advanceBy numFrames a

                              modify $ \s ->
                                  s { managerStateAnimations = HM.insert (animationID a') a' $
                                                               managerStateAnimations s
                                    }

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

    as <- gets (HM.elems . managerStateAnimations)
    go Nothing as

advanceBy :: Integer -> Animation s -> Animation s
advanceBy n a
    | n <= 0 = a
    | otherwise =
        advanceBy (n - 1) $
        advanceByOne a

advanceByOne :: Animation s -> Animation s
advanceByOne a =
    case animationMode a of
        Forward ->
            if animationCurrentFrame a == animationNumFrames a - 1
            then case animationDuration a of
                Loop -> a { animationCurrentFrame = 0
                          }
                Once -> a
            else a { animationCurrentFrame = animationCurrentFrame a + 1
                   }

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
    idVar <- STM.newTVarIO $ AnimationID 1
    reqTid <- forkIO $ animationManagerThreadBody inChan outChan mkEvent idVar
    tickTid <- forkIO $ tickThreadBody inChan
    runningVar <- STM.newTVarIO True
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

startAnimation :: AnimationManager s e n
               -> Int
               -> Integer
               -> AnimationMode
               -> Duration
               -> Traversal' s (Maybe Int)
               -> IO ()
startAnimation mgr numFrames frameMs mode duration updater =
    tellAnimationManager mgr $ StartAnimation numFrames frameMs mode duration updater

main :: IO ()
main = do
    chan <- newBChan 10

    void $ forkIO $ forever $ do
        writeBChan chan Counter
        threadDelay 1000000

    void $ customMainWithDefaultVty (Just chan) theApp initialState
