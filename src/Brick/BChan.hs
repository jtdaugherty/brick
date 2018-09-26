module Brick.BChan
  ( BChan
  , newBChan
  , writeBChan
  , readBChan
  , readBChan2
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

import Control.Concurrent.STM.TBQueue
import Control.Monad.STM (atomically, orElse)

-- | @BChan@ is an abstract type representing a bounded FIFO channel.
data BChan a = BChan (TBQueue a)

-- |Builds and returns a new instance of @BChan@.
newBChan :: Int   -- ^ maximum number of elements the channel can hold
          -> IO (BChan a)
newBChan size = atomically $ BChan <$> newTBQueue (fromIntegral size)

-- |Writes a value to a @BChan@; blocks if the channel is full.
writeBChan :: BChan a -> a -> IO ()
writeBChan (BChan q) a = atomically $ writeTBQueue q a

-- |Reads the next value from the @BChan@; blocks if necessary.
readBChan :: BChan a -> IO a
readBChan (BChan q) = atomically $ readTBQueue q

-- |Reads the next value from either @BChan@, prioritizing the first @BChan@;
-- blocks if necessary.
readBChan2 :: BChan a -> BChan b -> IO (Either a b)
readBChan2 (BChan q1) (BChan q2) = atomically $
  (Left <$> readTBQueue q1) `orElse` (Right <$> readTBQueue q2)
