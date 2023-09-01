{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Brick.Types.Internal
  ( ScrollRequest(..)
  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL
  , Location(..)
  , locL
  , origin
  , TerminalLocation(..)
  , Viewport(..)
  , ViewportType(..)
  , RenderState(..)
  , Direction(..)
  , CursorLocation(..)
  , cursorLocationL
  , cursorLocationNameL
  , cursorLocationVisibleL
  , VScrollBarOrientation(..)
  , HScrollBarOrientation(..)
  , VScrollbarRenderer(..)
  , HScrollbarRenderer(..)
  , ClickableScrollbarElement(..)
  , Context(..)
  , ctxAttrMapL
  , ctxAttrNameL
  , ctxBorderStyleL
  , ctxDynBordersL
  , ctxVScrollBarOrientationL
  , ctxVScrollBarRendererL
  , ctxHScrollBarOrientationL
  , ctxHScrollBarRendererL
  , ctxVScrollBarShowHandlesL
  , ctxHScrollBarShowHandlesL
  , ctxVScrollBarClickableConstrL
  , ctxHScrollBarClickableConstrL
  , availWidthL
  , availHeightL
  , windowWidthL
  , windowHeightL

  , Size(..)

  , EventState(..)
  , VtyContext(..)
  , EventRO(..)
  , NextAction(..)
  , Result(..)
  , Extent(..)
  , Edges(..)
  , eTopL, eBottomL, eRightL, eLeftL
  , BorderSegment(..)
  , bsAcceptL, bsOfferL, bsDrawL
  , DynBorder(..)
  , dbStyleL, dbAttrL, dbSegmentsL
  , CacheInvalidateRequest(..)
  , BrickEvent(..)
  , RenderM
  , getContext
  , lookupReportedExtent
  , Widget(..)

  , rsScrollRequestsL
  , viewportMapL
  , clickableNamesL
  , reportedExtentsL
  , renderCacheL
  , observedNamesL
  , requestedVisibleNames_L
  , vpSize
  , vpLeft
  , vpTop
  , vpContentSize
  , imageL
  , cursorsL
  , extentsL
  , bordersL
  , visibilityRequestsL
  , emptyResult
  )
where

import Control.Concurrent (ThreadId)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Lens.Micro (_1, _2, Lens')
import Lens.Micro.Mtl (use)
import Lens.Micro.TH (makeLenses)
import qualified Data.Set as S
import qualified Data.Map as M
import Graphics.Vty (Vty, Event, Button, Modifier, DisplayRegion, Image, Attr, emptyImage)
import GHC.Generics
import Control.DeepSeq (NFData)

import Brick.BorderMap (BorderMap)
import qualified Brick.BorderMap as BM
import Brick.Types.Common
import Brick.Types.TH
import Brick.AttrMap (AttrName, AttrMap)
import Brick.Widgets.Border.Style (BorderStyle)

data ScrollRequest = HScrollBy Int
                   | HScrollPage Direction
                   | HScrollToBeginning
                   | HScrollToEnd
                   | VScrollBy Int
                   | VScrollPage Direction
                   | VScrollToBeginning
                   | VScrollToEnd
                   | SetTop Int
                   | SetLeft Int
                   deriving (Read, Show, Generic, NFData)

-- | Widget size policies. These policies communicate how a widget uses
-- space when being rendered. These policies influence rendering order
-- and space allocation in the box layout algorithm for 'hBox' and
-- 'vBox'.
data Size = Fixed
          -- ^ Widgets advertising this size policy should take up the
          -- same amount of space no matter how much they are given,
          -- i.e. their size depends on their contents alone rather than
          -- on the size of the rendering area.
          | Greedy
          -- ^ Widgets advertising this size policy must take up all the
          -- space they are given.
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

data RenderState n =
    RS { viewportMap :: !(M.Map n Viewport)
       , rsScrollRequests :: ![(n, ScrollRequest)]
       , observedNames :: !(S.Set n)
       , renderCache :: !(M.Map n ([n], Result n))
       , clickableNames :: ![n]
       , requestedVisibleNames_ :: !(S.Set n)
       , reportedExtents :: !(M.Map n (Extent n))
       } deriving (Read, Show, Generic, NFData)

-- | The type of the rendering monad. This monad is used by the
-- library's rendering routines to manage rendering state and
-- communicate rendering parameters to widgets' rendering functions.
type RenderM n a = ReaderT (Context n) (State (RenderState n)) a

-- | Get the current rendering context.
getContext :: RenderM n (Context n)
getContext = ask

-- | Orientations for vertical scroll bars.
data VScrollBarOrientation = OnLeft | OnRight
                           deriving (Show, Eq)

-- | Orientations for horizontal scroll bars.
data HScrollBarOrientation = OnBottom | OnTop
                           deriving (Show, Eq)

-- | A vertical scroll bar renderer.
data VScrollbarRenderer n =
    VScrollbarRenderer { renderVScrollbar :: Widget n
                       -- ^ How to render the body of the scroll bar.
                       -- This should provide a widget that expands in
                       -- whatever direction(s) this renderer will be
                       -- used for. So, for example, this widget would
                       -- need to be one that expands vertically such as
                       -- @fill@. The same goes for the trough widget.
                       , renderVScrollbarTrough :: Widget n
                       -- ^ How to render the "trough" of the scroll bar
                       -- (the area to either side of the scroll bar
                       -- body). This should expand as described in the
                       -- documentation for the scroll bar field.
                       , renderVScrollbarHandleBefore :: Widget n
                       -- ^ How to render the handle that appears at
                       -- the top or left of the scrollbar. The result
                       -- will be allowed to be at most one row high.
                       , renderVScrollbarHandleAfter :: Widget n
                       -- ^ How to render the handle that appears at the
                       -- bottom or right of the scrollbar. The result
                       -- will be allowed to be at most one row high.
                       , scrollbarWidthAllocation :: Int
                       -- ^ The number of columns that will be allocated
                       -- to the scroll bar. This determines how much
                       -- space the widgets of the scroll bar elements
                       -- can take up. If they use less than this
                       -- amount, padding will be applied between the
                       -- scroll bar and the viewport contents.
                       }

-- | A horizontal scroll bar renderer.
data HScrollbarRenderer n =
    HScrollbarRenderer { renderHScrollbar :: Widget n
                       -- ^ How to render the body of the scroll bar.
                       -- This should provide a widget that expands
                       -- in whatever direction(s) this renderer will
                       -- be used for. So, for example, this widget
                       -- would need to be one that expands horizontally
                       -- such as @fill@. The same goes for the trough
                       -- widget.
                       , renderHScrollbarTrough :: Widget n
                       -- ^ How to render the "trough" of the scroll bar
                       -- (the area to either side of the scroll bar
                       -- body). This should expand as described in the
                       -- documentation for the scroll bar field.
                       , renderHScrollbarHandleBefore :: Widget n
                       -- ^ How to render the handle that appears at the
                       -- top or left of the scrollbar. The result will
                       -- be allowed to be at most one column wide.
                       , renderHScrollbarHandleAfter :: Widget n
                       -- ^ How to render the handle that appears at the
                       -- bottom or right of the scrollbar. The result
                       -- will be allowed to be at most one column wide.
                       , scrollbarHeightAllocation :: Int
                       -- ^ The number of rows that will be allocated to
                       -- the scroll bar. This determines how much space
                       -- the widgets of the scroll bar elements can
                       -- take up. If they use less than this amount,
                       -- padding will be applied between the scroll bar
                       -- and the viewport contents.
                       }

data VisibilityRequest =
    VR { vrPosition :: Location
       , vrSize :: DisplayRegion
       }
       deriving (Show, Eq, Read, Generic, NFData)

-- | Describes the state of a viewport as it appears as its most recent
-- rendering.
data Viewport =
    VP { _vpLeft :: Int
       -- ^ The column offset of left side of the viewport.
       , _vpTop :: Int
       -- ^ The row offset of the top of the viewport.
       , _vpSize :: DisplayRegion
       -- ^ The size of the viewport.
       , _vpContentSize :: DisplayRegion
       -- ^ The size of the contents of the viewport.
       }
       deriving (Show, Read, Generic, NFData)

-- | The type of viewports that indicates the direction(s) in which a
-- viewport is scrollable.
data ViewportType =
    Vertical
    -- ^ Viewports of this type are scrollable only vertically.
    | Horizontal
    -- ^ Viewports of this type are scrollable only horizontally.
    | Both
    -- ^ Viewports of this type are scrollable vertically and horizontally.
    deriving (Show, Eq)

data CacheInvalidateRequest n =
    InvalidateSingle n
    | InvalidateEntire
    deriving (Ord, Eq)

data EventState n =
    ES { esScrollRequests :: ![(n, ScrollRequest)]
       , cacheInvalidateRequests :: !(S.Set (CacheInvalidateRequest n))
       , requestedVisibleNames :: !(S.Set n)
       , nextAction :: !NextAction
       , vtyContext :: VtyContext
       }

data VtyContext =
    VtyContext { vtyContextBuilder :: IO Vty
               , vtyContextHandle :: Vty
               , vtyContextThread :: ThreadId
               , vtyContextPutEvent :: Event -> IO ()
               }

-- | An extent of a named area: its size, location, and origin.
data Extent n = Extent { extentName      :: !n
                       , extentUpperLeft :: !Location
                       , extentSize      :: !(Int, Int)
                       }
                       deriving (Show, Read, Generic, NFData)

-- | The type of actions to take upon completion of an event handler.
data NextAction =
    Continue
    | ContinueWithoutRedraw
    | Halt

-- | Scrolling direction.
data Direction = Up
               -- ^ Up/left
               | Down
               -- ^ Down/right
               deriving (Show, Eq, Read, Generic, NFData)

-- | The class of types that behave like terminal locations.
class TerminalLocation a where
    -- | Get the column out of the value
    locationColumnL :: Lens' a Int
    locationColumn :: a -> Int

    -- | Get the row out of the value
    locationRowL :: Lens' a Int
    locationRow :: a -> Int

instance TerminalLocation Location where
    locationColumnL = _1
    locationColumn (Location t) = fst t
    locationRowL = _2
    locationRow (Location t) = snd t

-- | A cursor location.  These are returned by the rendering process.
data CursorLocation n =
    CursorLocation { cursorLocation :: !Location
                   -- ^ The location
                   , cursorLocationName :: !(Maybe n)
                   -- ^ The name of the widget associated with the location
                   , cursorLocationVisible :: !Bool
                   -- ^ Whether the cursor should actually be visible
                   }
                   deriving (Read, Show, Generic, NFData)

-- | A border character has four segments, one extending in each direction
-- (horizontally and vertically) from the center of the character.
data BorderSegment = BorderSegment
    { bsAccept :: Bool
    -- ^ Would this segment be willing to be drawn if a neighbor wanted to
    -- connect to it?
    , bsOffer :: Bool
    -- ^ Does this segment want to connect to its neighbor?
    , bsDraw :: Bool
    -- ^ Should this segment be represented visually?
    } deriving (Eq, Ord, Read, Show, Generic, NFData)

-- | Information about how to redraw a dynamic border character when it abuts
-- another dynamic border character.
data DynBorder = DynBorder
    { dbStyle :: BorderStyle
    -- ^ The 'Char's to use when redrawing the border. Also used to filter
    -- connections: only dynamic borders with equal 'BorderStyle's will connect
    -- to each other.
    , dbAttr :: Attr
    -- ^ What 'Attr' to use to redraw the border character. Also used to filter
    -- connections: only dynamic borders with equal 'Attr's will connect to
    -- each other.
    , dbSegments :: Edges BorderSegment
    } deriving (Eq, Read, Show, Generic, NFData)

-- | The type of result returned by a widget's rendering function. The
-- result provides the image, cursor positions, and visibility requests
-- that resulted from the rendering process.
data Result n =
    Result { image :: !Image
           -- ^ The final rendered image for a widget
           , cursors :: ![CursorLocation n]
           -- ^ The list of reported cursor positions for the
           -- application to choose from
           , visibilityRequests :: ![VisibilityRequest]
           -- ^ The list of visibility requests made by widgets rendered
           -- while rendering this one (used by viewports)
           , extents :: ![Extent n]
           -- Programmer's note: we don't try to maintain the invariant that
           -- the size of the borders closely matches the size of the 'image'
           -- field. Most widgets don't need to care about borders, and so they
           -- use the empty 'BorderMap' that has a degenerate rectangle. Only
           -- border-drawing widgets and the hbox/vbox stuff try to set this
           -- carefully. Even then, in the boxes, we only make sure that the
           -- 'BorderMap' is no larger than the entire concatenation of boxes,
           -- and it's certainly possible for it to be smaller. (Resizing
           -- 'BorderMap's is lossy, so we try to do it as little as possible.)
           -- If you're writing a widget, this should make it easier for you to
           -- do so; but beware this lack of invariant if you are consuming
           -- widgets.
           , borders :: !(BorderMap DynBorder)
           -- ^ Places where we may rewrite the edge of the image when
           -- placing this widget next to another one.
           }
           deriving (Show, Read, Generic, NFData)

emptyResult :: Result n
emptyResult =
    Result { image = emptyImage
           , cursors = []
           , visibilityRequests = []
           , extents = []
           , borders = BM.empty
           }

-- | The type of events.
data BrickEvent n e = VtyEvent Event
                    -- ^ The event was a Vty event.
                    | AppEvent e
                    -- ^ The event was an application event.
                    | MouseDown n Button [Modifier] Location
                    -- ^ A mouse-down event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    | MouseUp n (Maybe Button) Location
                    -- ^ A mouse-up event on the specified region was
                    -- received. The 'n' value is the resource name of
                    -- the clicked widget (see 'clickable').
                    deriving (Show, Eq, Ord)

data EventRO n = EventRO { eventViewportMap :: M.Map n Viewport
                         , latestExtents :: [Extent n]
                         , oldState :: RenderState n
                         }

-- | Clickable elements of a scroll bar.
data ClickableScrollbarElement =
    SBHandleBefore
    -- ^ The handle at the beginning (left/top) of the scroll bar.
    | SBHandleAfter
    -- ^ The handle at the end (right/bottom) of the scroll bar.
    | SBBar
    -- ^ The scroll bar itself.
    | SBTroughBefore
    -- ^ The trough before the scroll bar.
    | SBTroughAfter
    -- ^ The trough after the scroll bar.
    deriving (Eq, Show, Ord)

-- | The rendering context. This tells widgets how to render: how much
-- space they have in which to render, which attribute they should use
-- to render, which bordering style should be used, and the attribute map
-- available for rendering.
data Context n =
    Context { ctxAttrName :: AttrName
            , availWidth :: Int
            , availHeight :: Int
            , windowWidth :: Int
            , windowHeight :: Int
            , ctxBorderStyle :: BorderStyle
            , ctxAttrMap :: AttrMap
            , ctxDynBorders :: Bool
            , ctxVScrollBarOrientation :: Maybe VScrollBarOrientation
            , ctxVScrollBarRenderer :: Maybe (VScrollbarRenderer n)
            , ctxHScrollBarOrientation :: Maybe HScrollBarOrientation
            , ctxHScrollBarRenderer :: Maybe (HScrollbarRenderer n)
            , ctxVScrollBarShowHandles :: Bool
            , ctxHScrollBarShowHandles :: Bool
            , ctxVScrollBarClickableConstr :: Maybe (ClickableScrollbarElement -> n -> n)
            , ctxHScrollBarClickableConstr :: Maybe (ClickableScrollbarElement -> n -> n)
            }

suffixLenses ''RenderState
suffixLenses ''VisibilityRequest
suffixLenses ''CursorLocation
suffixLenses ''Context
suffixLenses ''DynBorder
suffixLenses ''Result
suffixLenses ''BorderSegment
makeLenses ''Viewport

lookupReportedExtent :: (Ord n) => n -> RenderM n (Maybe (Extent n))
lookupReportedExtent n = do
    m <- lift $ use reportedExtentsL
    return $ M.lookup n m
