-- | Basic types used by this library.
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
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
  , EventM
  , BrickEvent(..)
  , withLens
  , nestEventM

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

import Lens.Micro (_1, _2, to, (^.), Lens')
import Lens.Micro.Type (Getting)
import Lens.Micro.Mtl ((.=), use)
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif
import Control.Monad.State.Strict
import Control.Monad.Reader
import Graphics.Vty (Attr)

import Brick.Types.TH
import Brick.Types.Internal
import Brick.Types.EventM
import Brick.AttrMap (AttrName, attrMapLookup)

-- | The type of padding.
data Padding = Pad Int
             -- ^ Pad by the specified number of rows or columns.
             | Max
             -- ^ Pad up to the number of available rows or columns.

-- | Given a state value and an 'EventM' that mutates that state, run
-- the specified action and return both the resulting modified state and
-- the result of the action itself.
nestEventM :: a
           -- ^ The lens to use to extract and store the state mutated
           -- by the action.
           -> EventM n a b
           -- ^ The action to run.
           -> EventM n s (a, b)
nestEventM s' act = do
    ro <- EventM ask
    s <- EventM $ lift get
    let stInner = ES { applicationState = s'
                     , nextAction = Continue
                     , esScrollRequests = esScrollRequests s
                     , cacheInvalidateRequests = cacheInvalidateRequests s
                     , requestedVisibleNames = requestedVisibleNames s
                     }
    (actResult, stInnerFinal) <- liftIO $ runStateT (runReaderT (runEventM act) ro) stInner

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
    return (finalSt, actResult)

-- | Given a lens into a field of the current state, focus mutations on
-- the state field itself.
withLens :: Lens' s a
         -- ^ The lens to use to extract and store the state
         -- mutated by the action.
         -> EventM n a b
         -- ^ The action to run, scoped over some state to manage.
         -> EventM n s b
withLens target act = do
    val <- use target
    (val', result) <- nestEventM val act
    target .= val'
    return result

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
