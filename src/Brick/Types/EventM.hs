{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Brick.Types.EventM
  ( EventM(..)
  , getVtyContext
  )
where

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Control.Monad.Reader
import Control.Monad.State.Strict
import Lens.Micro.Mtl
import Lens.Micro.Mtl.Internal

import Brick.Types.Internal

-- | The monad in which event handlers run.
newtype EventM n s a =
    EventM { runEventM :: ReaderT (EventRO n) (StateT s (StateT (EventState n) IO)) a
           }
           deriving ( Functor, Applicative, Monad, MonadIO
                    , MonadThrow, MonadCatch, MonadMask
#if !MIN_VERSION_base(4,13,0)
                    , MonadFail
#endif
                    )

instance MonadState s (EventM n s) where
    get = EventM $ lift get
    put = EventM . lift . put

getVtyContext :: EventM n s VtyContext
getVtyContext = EventM $ lift $ lift $ gets vtyContext

type instance Zoomed (EventM n s) = Zoomed (StateT s (StateT (EventState n) IO))

instance Zoom (EventM n s) (EventM n t) s t where
    zoom l (EventM m) = EventM (zoom l m)
