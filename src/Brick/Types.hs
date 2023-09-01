-- | Basic types used by this library.
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
  , VScrollbarRenderer(..)
  , HScrollbarRenderer(..)
  , ClickableScrollbarElement(..)

  -- * Event-handling types and functions
  , EventM
  , BrickEvent(..)
  , nestEventM
  , nestEventM'

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
  , Direction(..)

  -- * Renderer internals (for benchmarking)
  , RenderState

  -- * Re-exports for convenience
  , get
  , gets
  , put
  , modify
  , zoom
  )
where

import Lens.Micro (_1, _2, to, (^.))
import Lens.Micro.Type (Getting)
import Lens.Micro.Mtl (zoom)
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

-- | Given a state value and an 'EventM' that mutates that state, run
-- the specified action and return resulting modified state.
nestEventM' :: a
           -- ^ The initial state to use in the nested action.
            -> EventM n a b
            -- ^ The action to run.
            -> EventM n s a
nestEventM' s act = fst <$> nestEventM s act

-- | Given a state value and an 'EventM' that mutates that state, run
-- the specified action and return both the resulting modified state and
-- the result of the action itself.
nestEventM :: a
           -- ^ The initial state to use in the nested action.
           -> EventM n a b
           -- ^ The action to run.
           -> EventM n s (a, b)
nestEventM s' act = do
    ro <- EventM ask
    es <- EventM $ lift $ lift get
    vtyCtx <- getVtyContext
    let stInner = ES { nextAction = Continue
                     , esScrollRequests = esScrollRequests es
                     , cacheInvalidateRequests = cacheInvalidateRequests es
                     , requestedVisibleNames = requestedVisibleNames es
                     , vtyContext = vtyCtx
                     }
    ((actResult, newSt), stInnerFinal) <- liftIO $ runStateT (runStateT (runReaderT (runEventM act) ro) s') stInner

    EventM $ lift $ lift $ modify $
        \st -> st { nextAction = nextAction stInnerFinal
                  , esScrollRequests = esScrollRequests stInnerFinal
                  , cacheInvalidateRequests = cacheInvalidateRequests stInnerFinal
                  , requestedVisibleNames = requestedVisibleNames stInnerFinal
                  , vtyContext = vtyContext stInnerFinal
                  }
    return (newSt, actResult)

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
