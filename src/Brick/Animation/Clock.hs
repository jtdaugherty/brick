-- | This module provides an API for working with
-- 'Data.Time.Clock.System.SystemTime' values similar to that of
-- 'Data.Time.Clock.UTCTime'. @SystemTime@s are more efficient to
-- obtain than @UTCTime@s, which is important to avoid animation
-- tick thread delays associated with expensive clock reads. In
-- addition, the @UTCTime@-based API provides unpleasant @Float@-based
-- conversions. Since the @SystemTime@-based API doesn't provide some
-- of the operations we need, and since it is easier to work with at
-- millisecond granularity, it is extended here for internal use.
module Brick.Animation.Clock
  ( Time
  , getTime
  , addOffset
  , subtractTime

  , Offset
  , offsetFromMs
  , offsetToMs
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Time.Clock.System as C

newtype Time = Time C.SystemTime
             deriving (Ord, Eq)

-- | Signed difference in milliseconds
newtype Offset = Offset Integer
               deriving (Ord, Eq)

offsetFromMs :: Integer -> Offset
offsetFromMs = Offset

offsetToMs :: Offset -> Integer
offsetToMs (Offset ms) = ms

getTime :: (MonadIO m) => m Time
getTime = Time <$> liftIO C.getSystemTime

addOffset :: Offset -> Time -> Time
addOffset (Offset ms) (Time (C.MkSystemTime s ns)) =
    Time $ C.MkSystemTime (fromInteger s') (fromInteger ns')
    where
        -- Note that due to the behavior of divMod, this works even when
        -- the offset is negative: the number of seconds is decremented
        -- and the remainder of nanoseconds is correct.
        s' = newSec + toInteger s
        (newSec, ns') = (nsPerMs * ms + toInteger ns)
                          `divMod` (msPerS * nsPerMs)

subtractTime :: Time -> Time -> Offset
subtractTime t1 t2 = Offset $ timeToMs t1 - timeToMs t2

timeToMs :: Time -> Integer
timeToMs (Time (C.MkSystemTime s ns)) =
    (toInteger s) * msPerS +
    (toInteger ns) `div` nsPerMs

nsPerMs :: Integer
nsPerMs = 1000000

msPerS :: Integer
msPerS = 1000
