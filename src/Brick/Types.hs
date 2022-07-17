-- | Basic types used by this library.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
  , vpContentSize
  , VScrollBarOrientation(..)
  , HScrollBarOrientation(..)
  , ScrollbarRenderer(..)
  , ClickableScrollbarElement(..)

  -- * Event-handling types
  , EventM(..)
  , BrickEvent(..)
  , handleEventLensed
  , updateWithLens
  , runEventMWithState

  -- * Rendering infrastructure
  , RenderM
  , getContext

  -- ** The rendering context
  , Context(ctxAttrName, availWidth, availHeight, windowWidth, windowHeight, ctxBorderStyle, ctxAttrMap, ctxDynBorders)
  , attrL
  , availWidthL
  , availHeightL
  , windowWidthL
  , windowHeightL
  , ctxVScrollBarOrientationL
  , ctxVScrollBarRendererL
  , ctxHScrollBarOrientationL
  , ctxHScrollBarRendererL
  , ctxAttrMapL
  , ctxAttrNameL
  , ctxBorderStyleL
  , ctxDynBordersL

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
  , suffixLensesWith

  -- * Dynamic borders
  , bordersL
  , DynBorder(..)
  , dbStyleL, dbAttrL, dbSegmentsL
  , BorderSegment(..)
  , bsAcceptL, bsOfferL, bsDrawL
  , Edges(..)
  , eTopL, eBottomL, eRightL, eLeftL

  -- * Miscellaneous
  , Size(..)
  , Padding(..)
  , Direction(..)

  -- * Renderer internals (for benchmarking)
  , RenderState

  -- * Re-exports for convenience
  , get
  , gets
  , put
  , modify
  )
where

import Lens.Micro (_1, _2, to, (^.), (&), (.~), Lens')
import Lens.Micro.Type (Getting)
import Lens.Micro.Mtl ((.=), use)
import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Control.Monad.State.Strict
import Control.Monad.Reader
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
handleEventLensed :: Lens' s a
                  -- ^ The lens to use to extract and store the target
                  -- of the event.
                  -> (e -> EventM n a ())
                  -- ^ The event handler.
                  -> e
                  -- ^ The event to handle.
                  -> EventM n s ()
handleEventLensed target handleEvent ev =
    updateWithLens target (handleEvent ev)

runEventMWithState :: a
                   -- ^ The lens to use to extract and store the state
                   -- mutated by the action.
                   -> EventM n a ()
                   -- ^ The action to run.
                   -> EventM n s a
runEventMWithState s' act = do
    ro <- EventM ask
    s <- EventM $ lift get
    let stInner = ES { applicationState = s'
                     , nextAction = Continue
                     , esScrollRequests = esScrollRequests s
                     , cacheInvalidateRequests = cacheInvalidateRequests s
                     , requestedVisibleNames = requestedVisibleNames s
                     }
    ((), stInnerFinal) <- liftIO $ runStateT (runReaderT (runEventM act) ro) stInner

    (nextAct, finalSt) <- case nextAction stInnerFinal of
        Continue ->
            return (Continue, applicationState stInnerFinal)
        ContinueWithoutRedraw ->
            return (ContinueWithoutRedraw, applicationState stInnerFinal)
        Halt ->
            return (Halt, applicationState stInnerFinal)
        SuspendAndResume act' -> do
            s'' <- liftIO act'
            return (Continue, s'')

    EventM $ lift $ modify $ \st -> st { nextAction = nextAct
                                       , esScrollRequests = esScrollRequests stInnerFinal
                                       , cacheInvalidateRequests = cacheInvalidateRequests stInnerFinal
                                       , requestedVisibleNames = requestedVisibleNames stInnerFinal
                                       }
    return finalSt

updateWithLens :: Lens' s a
               -- ^ The lens to use to extract and store the state
               -- mutated by the action.
               -> EventM n a ()
               -- ^ The action to run.
               -> EventM n s ()
updateWithLens target act = do
    val <- use target
    val' <- runEventMWithState val act
    target .= val'

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

-- | The rendering context's current drawing attribute.
attrL :: forall r n. Getting r (Context n) Attr
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
