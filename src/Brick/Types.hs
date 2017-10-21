-- | Basic types used by this library.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Brick.Types
  ( -- * The Widget type
    Widget(..)

    -- * Location types and lenses
  , Location(..)
  , locL
  , TerminalLocation(..)
  , CursorLocation(..)
  , cursorLocationL
  , cursorLocationNameL

  -- * Viewports
  , Viewport(..)
  , ViewportType(..)
  , vpSize
  , vpTop
  , vpLeft

  -- * Event-handling types
  , EventM(..)
  , EventNext(..)
  , liftRawEventHandler
  , EventHandler(..)
  , BrickEvent(..)
  , handleEventLensed

  -- * Rendering infrastructure
  , RenderM
  , getContext

  -- ** The rendering context
  , Context(ctxAttrName, availWidth, availHeight, ctxBorderStyle, ctxAttrMap)
  , attrL
  , availWidthL
  , availHeightL
  , ctxAttrMapL
  , ctxAttrNameL
  , ctxBorderStyleL

  -- ** Rendering results
  , Result(..)
  , emptyResult
  , lookupAttrName
  , Extent(..)

  -- ** Rendering result lenses
  , imageL
  , cursorsL
  , visibilityRequestsL
  , extentsL

  -- ** Visibility requests
  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL

  -- * Making lenses
  , suffixLenses

  -- * Miscellaneous
  , Size(..)
  , Padding(..)
  , Direction(..)

  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid (Monoid(..))
#endif

import Lens.Micro (_1, _2, to, (^.), (&), (.~), Lens')
import Lens.Micro.Type (Getting)
import Control.Monad.State
import Control.Monad.Trans.Reader
import Graphics.Vty (Attr)

import Brick.Types.TH
import Brick.Types.Internal
import Brick.AttrMap (AttrName, attrMapLookup)

-- | The type of padding.
data Padding = Pad Int
             -- ^ Pad by the specified number of rows or columns.
             | Max
             -- ^ Pad up to the number of available rows or columns.

-- | A convenience function for handling events intended for values
-- that are targets of lenses in your application state. This function
-- obtains the target value of the specified lens, invokes 'handleEvent'
-- on it, and stores the resulting transformed value back in the state
-- using the lens.
handleEventLensed :: a
                  -- ^ The state value.
                  -> Lens' a b
                  -- ^ The lens to use to extract and store the target
                  -- of the event.
                  -> (e -> b -> EventM n s b)
                  -- ^ The event handler.
                  -> e
                  -- ^ The event to handle.
                  -> EventM n s a
handleEventLensed v target handleEvent ev = do
    newB <- handleEvent ev (v^.target)
    return $ v & target .~ newB

newtype EventM n s a = EventM
    { runEventM :: s -> (s, EventNext n s a)
    }

instance Functor (EventM n s) where
    fmap f (EventM func) = EventM $ \s ->
      let (s', en) = func s
      in (s', f <$> en)

instance Applicative (EventM n s) where
    pure a = EventM $ \s -> (s, pure a)
    (EventM f1) <*> e2@(EventM f2) = EventM $ \s ->
      let (s', enfa) = f1 s
      in case enfa of
        Halt -> (s', Halt)
        Pure f ->
          let (s'', ena) = f2 s'
          in (s'', f <$> ena)
        Resume eh -> (s', Resume $ do
          emf <- eh
          pure $ emf <*> e2)
        SuspendAndResume iof -> (s', SuspendAndResume $ do
          ehemf <- iof
          pure $ ehemf <*> e2)

instance Monad (EventM n s) where
    (EventM efa) >>= f = EventM $ \s ->
      let (s', ena) = efa s
      in case ena of
        Halt -> (s', Halt)
        Pure a ->
          let EventM eb = f a
          in eb s'
        Resume eh -> (s', Resume $ do
          emf <- eh
          pure $ emf >>= f)
        SuspendAndResume iof -> (s', SuspendAndResume $ do
          ehemf <- iof
          pure $ ehemf >>= f)

instance MonadIO (EventM n s) where
    liftIO act = EventM $ \s -> (s, liftIO act)

instance MonadState s (EventM n s) where
    get = EventM $ \s -> (s, pure s)
    put s = EventM $ \_ -> (s, pure ())
    state func = EventM $ \s ->
      let (v, s') = func s
      in (s', pure v)

data EventNext n s a
    = Halt
    | Pure a
    | Resume (EventHandler n (EventM n s a))
    | SuspendAndResume (IO (EventM n s a))

instance Functor (EventNext n s) where
    fmap _ Halt = Halt
    fmap f (Pure a) = Pure $ f a
    fmap f (Resume eh) = Resume $ fmap (f <$>) eh
    fmap f (SuspendAndResume ioeh) = SuspendAndResume $ fmap (f <$>) ioeh

instance Applicative (EventNext n s) where
    pure = Pure
    Halt <*> _ = Halt
    Pure f <*> e2 = f <$> e2
    Resume ehf <*> e2 = Resume $ do
      -- (EventM n s (a -> b))
      -- (EventNext n s a)
      ef <- ehf
      pure $ ef <*> EventM (\s -> (s, e2))
    SuspendAndResume ehf <*> e2 = SuspendAndResume $ do
      eh <- ehf
      pure $ eh <*> EventM (\s -> (s, e2))

instance Monad (EventNext n s) where
    Halt >>= _ = Halt
    Pure a >>= f = f a
    Resume eha >>= ef = Resume $ do
      ea <- eha
      pure $ ea >>= (\a -> EventM (\s -> (s, ef a)))
    SuspendAndResume eha >>= ef = SuspendAndResume $ do
      ea <- eha
      pure $ ea >>= (\a -> EventM (\s -> (s, ef a)))
--
instance MonadIO (EventNext n s) where
    liftIO act = Resume $ do
      a <- liftIO act -- EventHandler n s a
      pure $ pure a

liftRawEventHandler :: ReaderT (EventRO n) (StateT (EventState n) IO) a
                 -> EventM n s a
liftRawEventHandler eh = EventM $ \s ->
    (s, Resume $ EventHandler eh >>= (pure . pure))

-- | The monad in which event handlers run. Although it may be tempting
-- to dig into the reader value yourself, just use
-- 'Brick.Main.lookupViewport'.
newtype EventHandler n a = EventHandler
    { runEventHandler :: ReaderT (EventRO n) (StateT (EventState n) IO) a
    } deriving (Functor, Applicative, Monad, MonadIO)


-- | Widget growth policies. These policies communicate to layout
-- algorithms how a widget uses space when being rendered. These
-- policies influence rendering order and space allocation in the box
-- layout algorithm.
data Size = Fixed
          -- ^ Fixed widgets take up the same amount of space no matter
          -- how much they are given (non-greedy).
          | Greedy
          -- ^ Greedy widgets take up all the space they are given.
          deriving (Show, Eq, Ord)

-- | The type of widgets.
data Widget n =
    Widget { hSize :: Size
           -- ^ This widget's horizontal growth policy
           , vSize :: Size
           -- ^ This widget's vertical growth policy
           , render :: RenderM n (Result n)
           -- ^ This widget's rendering function
           }

-- | The type of the rendering monad. This monad is used by the
-- library's rendering routines to manage rendering state and
-- communicate rendering parameters to widgets' rendering functions.
type RenderM n a = ReaderT Context (State (RenderState n)) a

-- | Get the current rendering context.
getContext :: RenderM n Context
getContext = ask

suffixLenses ''Context

-- | The rendering context's current drawing attribute.
attrL :: forall r. Getting r Context Attr
attrL = to (\c -> attrMapLookup (c^.ctxAttrNameL) (c^.ctxAttrMapL))

instance TerminalLocation (CursorLocation n) where
    locationColumnL = cursorLocationL._1
    locationColumn = locationColumn . cursorLocation
    locationRowL = cursorLocationL._2
    locationRow = locationRow . cursorLocation

-- | Given an attribute name, obtain the attribute for the attribute
-- name by consulting the context's attribute map.
lookupAttrName :: AttrName -> RenderM n Attr
lookupAttrName n = do
    c <- getContext
    return $ attrMapLookup n (c^.ctxAttrMapL)
