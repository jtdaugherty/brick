{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
module Brick.Types.EventM
  ( EventM(..)
  )
where

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Control.Monad.Reader
import Control.Monad.State.Strict

import Brick.Types.Internal

-- | The monad in which event handlers run. Although it may be tempting
-- to dig into the reader value yourself, just use
-- 'Brick.Main.lookupViewport'.
newtype EventM n s a =
    EventM { runEventM :: ReaderT (EventRO n) (StateT (EventState n s) IO) a
           }
           deriving ( Functor, Applicative, Monad, MonadIO
                    , MonadThrow, MonadCatch, MonadMask, MonadFail
                    )

instance MonadState s (EventM n s) where
    get = EventM $ lift $ gets applicationState
    put s = EventM $ lift $ modify $ \es -> es { applicationState = s }
