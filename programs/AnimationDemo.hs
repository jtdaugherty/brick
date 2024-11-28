{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Lens.Micro ((^.), (%~), (.~), (&), Traversal')
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (forever, when)
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
  ( str
  , vBox
  , hBox
  )

data AnimationManagerRequest s =
    Tick UTCTime
    | StartAnimation AnimationID Int Integer AnimationMode Duration (Traversal' s (Maybe Int))
    -- ^ ID, frame count, frame duration in milliseconds, mode, duration, updater
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

newtype AnimationID = AnimationID Int
                    deriving (Eq, Ord, Show, Hashable)

data AnimationState s =
    AnimationState { _animationID :: AnimationID
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
                   , animationFrameUpdater :: Traversal' s (Maybe Int)
                   , _animationNextFrameTime :: UTCTime
                   }

makeLenses ''AnimationState

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

setNextFrameTime :: UTCTime -> AnimationState s -> AnimationState s
setNextFrameTime t a = a & animationNextFrameTime .~ t

nominalDiffFromMs :: Integer -> NominalDiffTime
nominalDiffFromMs i = realToFrac (fromIntegral i / (1000.0::Float))

nominalDiffToMs :: NominalDiffTime -> Integer
nominalDiffToMs t =
    -- NOTE: probably wrong, but we'll have to find out what this gives us
    (round $ nominalDiffTimeToSeconds t)

data ManagerState s e n =
    ManagerState { _managerStateInChan :: STM.TChan (AnimationManagerRequest s)
                 , _managerStateOutChan :: BChan e
                 , _managerStateEventBuilder :: EventM n s () -> e
                 , _managerStateAnimations :: HM.HashMap AnimationID (AnimationState s)
                 }

makeLenses ''ManagerState

animationManagerThreadBody :: STM.TChan (AnimationManagerRequest s)
                           -> BChan e
                           -> (EventM n s () -> e)
                           -> IO ()
animationManagerThreadBody inChan outChan mkEvent =
    let initial = ManagerState { _managerStateInChan = inChan
                               , _managerStateOutChan = outChan
                               , _managerStateEventBuilder = mkEvent
                               , _managerStateAnimations = mempty
                               }
    in evalStateT runManager initial

type ManagerM s e n a = StateT (ManagerState s e n) IO a

getNextManagerRequest :: ManagerM s e n (AnimationManagerRequest s)
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

lookupAnimation :: AnimationID -> ManagerM s e n (Maybe (AnimationState s))
lookupAnimation aId =
    HM.lookup aId <$> use managerStateAnimations

insertAnimation :: AnimationState s -> ManagerM s e n ()
insertAnimation a =
    managerStateAnimations %= HM.insert (a^.animationID) a

getNextAnimationID :: (MonadIO m) => AnimationManager s e n -> m AnimationID
getNextAnimationID mgr = do
    let var = animationMgrNextAnimationID mgr
    liftIO $ STM.atomically $ do
        AnimationID i <- STM.readTVar var
        let next = AnimationID $ i + 1
        STM.writeTVar var next
        return next

runManager :: ManagerM s e n ()
runManager = forever $ do
    getNextManagerRequest >>= handleManagerRequest

handleManagerRequest :: AnimationManagerRequest s -> ManagerM s e n ()
handleManagerRequest (StartAnimation aId numFrames frameMs mode dur updater) = do
    now <- liftIO getCurrentTime
    let next = addUTCTime frameOffset now
        frameOffset = nominalDiffFromMs frameMs
        a = AnimationState { _animationID = aId
                           , _animationNumFrames = numFrames
                           , _animationCurrentFrame = 0
                           , _animationPreviousFrame = Nothing
                           , _animationFrameMilliseconds = frameMs
                           , _animationMode = mode
                           , _animationDuration = dur
                           , animationFrameUpdater = updater
                           , _animationNextFrameTime = next
                           }

    insertAnimation a
    sendApplicationEvent $ updater .= Just 0
handleManagerRequest (StopAnimation aId) = do
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

        updateFor a = animationFrameUpdater a .= Just (a^.animationCurrentFrame)

        go :: Maybe (EventM n s ()) -> [AnimationState s] -> ManagerM s e n (Maybe (EventM n s ()))
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

                              managerStateAnimations %= HM.insert (a'^.animationID) a'

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

advanceBy :: Integer -> AnimationState s -> AnimationState s
advanceBy n a
    | n <= 0 = a
    | otherwise =
        advanceBy (n - 1) $
        advanceByOne a

advanceByOne :: AnimationState s -> AnimationState s
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
    idVar <- STM.newTVarIO $ AnimationID 1
    reqTid <- forkIO $ animationManagerThreadBody inChan outChan mkEvent
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

tellAnimationManager :: (MonadIO m)
                     => AnimationManager s e n -> AnimationManagerRequest s -> m ()
tellAnimationManager mgr req =
    liftIO $
    STM.atomically $
    STM.writeTChan (animationMgrInputChan mgr) req

startAnimation :: (MonadIO m)
               => AnimationManager s e n
               -> Int
               -> Integer
               -> AnimationMode
               -> Duration
               -> Traversal' s (Maybe Int)
               -> m AnimationID
startAnimation mgr numFrames frameMs mode duration updater = do
    aId <- getNextAnimationID mgr
    tellAnimationManager mgr $ StartAnimation aId numFrames frameMs mode duration updater
    return aId

stopAnimation :: (MonadIO m)
              => AnimationManager s e n
              -> AnimationID
              -> m ()
stopAnimation mgr aId =
    tellAnimationManager mgr $ StopAnimation aId

data CustomEvent =
    AnimationUpdate (EventM () St ())

data St =
    St { _stAnimationManager :: AnimationManager St CustomEvent ()
       , _animation1ID :: Maybe AnimationID
       , _animation2ID :: Maybe AnimationID
       , _animation3ID :: Maybe AnimationID
       , _animation1Frame :: Maybe Int
       , _animation2Frame :: Maybe Int
       , _animation3Frame :: Maybe Int
       }

makeLenses ''St

drawUI :: St -> [Widget ()]
drawUI st = [drawAnimations st]

drawAnimations :: St -> Widget ()
drawAnimations st =
    vBox [ hBox [ drawAnimation frames1 $ st^.animation1Frame
                , str " "
                , drawAnimation frames2 $ st^.animation2Frame
                , str " "
                , drawAnimation frames3 $ st^.animation3Frame
                ]
         , vBox [ maybe (str " ") (const $ str "Animation #1 running") $ st^.animation1ID
                , maybe (str " ") (const $ str "Animation #2 running") $ st^.animation2ID
                , maybe (str " ") (const $ str "Animation #3 running") $ st^.animation3ID
                ]
         ]

frames1 :: [String]
frames1 = [".", "o", "O", "^", " "]

frames2 :: [String]
frames2 = ["|", "/", "-", "\\"]

frames3 :: [String]
frames3 = ["v", "-", "^", "-"]

drawAnimation :: [String] -> Maybe Int -> Widget ()
drawAnimation _ Nothing = str " "
drawAnimation frames (Just i) = str $ frames !! i

appEvent :: BrickEvent () CustomEvent -> EventM () St ()
appEvent e = do
    mgr <- use stAnimationManager
    case e of
        VtyEvent (V.EvKey V.KEsc []) -> halt
        VtyEvent (V.EvKey (V.KChar '1') []) -> do
            mOld <- use animation1ID
            case mOld of
                Just i -> stopAnimation mgr i >> animation1ID .= Nothing
                Nothing -> do
                    aId <- startAnimation mgr (length frames1) 1000 Forward Loop animation1Frame
                    animation1ID .= Just aId

        VtyEvent (V.EvKey (V.KChar '2') []) -> do
            mOld <- use animation2ID
            case mOld of
                Just i -> stopAnimation mgr i >> animation2ID .= Nothing
                Nothing -> do
                    aId <- startAnimation mgr (length frames2) 500 Forward Loop animation2Frame
                    animation2ID .= Just aId

        VtyEvent (V.EvKey (V.KChar '3') []) -> do
            mOld <- use animation3ID
            case mOld of
                Just i -> stopAnimation mgr i >> animation3ID .= Nothing
                Nothing -> do
                    aId <- startAnimation mgr (length frames3) 100 Forward Loop animation3Frame
                    animation3ID .= Just aId

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
    mgr <- startAnimationManager chan AnimationUpdate

    let initialState =
            St { _stAnimationManager = mgr
               , _animation1ID = Nothing
               , _animation2ID = Nothing
               , _animation3ID = Nothing
               , _animation1Frame = Nothing
               , _animation2Frame = Nothing
               , _animation3Frame = Nothing
               }

    (_, vty) <- customMainWithDefaultVty (Just chan) theApp initialState
    V.shutdown vty
