{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExplicitForAll #-}

module Brick.Types.EventM (
  EventM (..),
) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Reader
import Control.Monad.State.Strict

import Brick.Types.Internal

import Control.Monad.Identity (Identity)
import Lens.Micro
import Lens.Micro.Internal
import Lens.Micro.Mtl.Internal
import Lens.Micro.Mtl

-- | The monad in which event handlers run.
newtype EventM n s a = EventM
  { runEventM :: ReaderT (EventRO n) (StateT (EventState n s) IO) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFail
    )

instance MonadState s (EventM n s) where
  get = EventM $ lift $ gets applicationState
  put s = EventM $ lift $ modify $ \es -> es{applicationState = s}

type instance Zoomed (EventM n s) = Focusing IO

instance Zoom (EventM n s) (EventM n t) s t where
  zoom :: LensLike' (Focusing IO c) t s -> EventM n s c -> EventM n t c
  zoom l (EventM evm) = EventM (zoomOnReader evm)
   where
    zoomOnReader :: ReaderT e (StateT (EventState n s) IO) c -> ReaderT e (StateT (EventState n t) IO) c
    zoomOnReader (ReaderT rm) = ReaderT (zoomOnState . rm)
    zoomOnState :: StateT (EventState n s) IO c -> StateT (EventState n t) IO c
    zoomOnState (StateT sm) = StateT $ _ (uf _) -- TODO: This is where I got stuck
    uf m = unfocusing #. l (Focusing #. m)

appState :: Lens (EventState n s) (EventState n t) s t
appState = lens applicationState setter
 where
  setter :: EventState n s -> t -> EventState n t
  setter es t =
    ES
      { esScrollRequests = esScrollRequests es
      , cacheInvalidateRequests = cacheInvalidateRequests es
      , requestedVisibleNames = requestedVisibleNames es
      , applicationState = t
      , nextAction = nextA es t
      }
  nextA :: EventState n s -> t -> NextAction t
  nextA es t = case nextAction es of
    SuspendAndResume _ios -> SuspendAndResume $ return t
    Continue -> Continue
    ContinueWithoutRedraw -> ContinueWithoutRedraw
    Halt -> Halt
