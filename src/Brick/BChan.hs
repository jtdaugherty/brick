module Brick.BChan
  ( BChan
  , newBChan
  , writeBChan
  , writeBChanNonBlocking
  , readBChan
  , readBChan2
  )
where

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM (atomically, orElse)

-- | @BChan@ is an abstract type representing a bounded FIFO channel.
data BChan a = BChan (TBQueue a)

-- | Builds and returns a new instance of @BChan@.
newBChan :: Int   -- ^ maximum number of elements the channel can hold
          -> IO (BChan a)
newBChan size = atomically $ BChan <$> newTBQueue (fromIntegral size)

-- | Writes a value to a @BChan@; blocks if the channel is full.
writeBChan :: BChan a -> a -> IO ()
writeBChan (BChan q) a = atomically $ writeTBQueue q a

-- | Attempts to write a value to a @BChan@. If the channel has room,
-- the value is written and this returns 'True'. Otherwise this returns
-- 'False' and returns immediately.
writeBChanNonBlocking :: BChan a -> a -> IO Bool
writeBChanNonBlocking (BChan q) a = atomically $ do
    f <- isFullTBQueue q
    if f
       then return False
       else writeTBQueue q a >> return True

-- | Reads the next value from the @BChan@; blocks if necessary.
readBChan :: BChan a -> IO a
readBChan (BChan q) = atomically $ readTBQueue q

-- | Reads the next value from either @BChan@, prioritizing the first
-- @BChan@; blocks if necessary.
readBChan2 :: BChan a -> BChan b -> IO (Either a b)
readBChan2 (BChan q1) (BChan q2) = atomically $
  (Left <$> readTBQueue q1) `orElse` (Right <$> readTBQueue q2)
