{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.Internal
  ( Result(..)
  , image
  , cursors
  , attr
  , ctxAttrs
  , lookupAttrName
  , addResultOffset

  , RenderState(..)
  , ScrollRequest(..)
  , Direction(..)

  , renderFinal
  , Widget(..)
  , Size(..)
  , RenderM

  , Context
  , availW
  , availH
  , getActiveBorderStyle
  , getContext

  , withBorderStyle

  , ViewportType(..)

  , txt
  , str
  , fill

  , padLeft
  , padRight
  , padTop
  , padBottom

  , emptyWidget
  , hBox
  , vBox
  , (<=>)
  , (<+>)

  , hLimit
  , vLimit
  , withDefaultAttr
  , withAttr
  , forceAttr
  , updateAttrMap
  , raw
  , translateBy
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy
  , showCursor
  , viewport
  , visible
  , visibleRegion
  )
where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (.~), (&), (%~), to, _1, _2, view, each, to, ix)
import Control.Monad (when)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import qualified Data.Text as T
import Data.Default
import Data.Functor.Contravariant
import Data.Monoid ((<>), mempty)
import qualified Data.Map as M
import qualified Data.Function as DF
import Data.List (sortBy, partition)
import Control.Lens (Lens')
import Data.String (IsString(..))
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Widgets.Border.Style
import Brick.Util (clOffset)
import Brick.AttrMap
import Brick.Util (clamp)

data VisibilityRequest =
    VR { _vrPosition :: Location
       , _vrSize :: V.DisplayRegion
       }
       deriving Show

data ViewportType = Vertical | Horizontal deriving Show

data Viewport =
    VP { _vpLeft :: Int
       , _vpTop :: Int
       , _vpSize :: V.DisplayRegion
       }
       deriving Show

-- | The type of result returned by a widget's rendering function. The
-- result provides the image, cursor positions, and visibility requests
-- that resulted from the rendering process.
data Result =
    Result { _image :: V.Image
           -- ^ The final rendered image for a widget
           , _cursors :: [CursorLocation]
           -- ^ The list of reported cursor positions for the
           -- application to choose from
           , _visibilityRequests :: [VisibilityRequest]
           -- ^ The list of visibility requests made by widgets rendered
           -- while rendering this one (used by viewports)
           }
           deriving Show

-- | The rendering context. This tells widgets how to render: how much
-- space they have in which to render, which attribute they should use
-- to render, which bordring style should be used, and the attribute map
-- available for rendering.
data Context =
    Context { _ctxAttrName :: AttrName
            , _availW :: Int
            , _availH :: Int
            , _activeBorderStyle :: BorderStyle
            , _ctxAttrs :: AttrMap
            }

-- | The type of the rendering monad. This monad is used by the
-- library's rendering routines to manage rendering state and
-- communicate rendering parameters to widgets' rendering functions.
type RenderM a = ReaderT Context (State RenderState) a

-- | Widget growth policies.
data Size = Fixed
          -- ^ Fixed widgets take up the same amount of space no matter
          -- how much they are given.
          | Unlimited
          -- ^ Unlimited widgets take up the space they are given.
          deriving (Show, Eq, Ord)

-- | The type of widgets.
data Widget =
    Widget { hSize :: Size
           -- ^ This widget's horizontal growth policy
           , vSize :: Size
           -- ^ This widget's vertical growth policy
           , render :: RenderM Result
           -- ^ This widget's rendering function
           }

data Direction = Up | Down

data ScrollRequest = ScrollBy Int
                   | ScrollPage Direction
                   | ScrollToBeginning
                   | ScrollToEnd

data RenderState =
    RS { _viewportMap :: M.Map Name Viewport
       , _scrollRequests :: [(Name, ScrollRequest)]
       }

makeLenses ''Result
makeLenses ''Context
makeLenses ''VisibilityRequest
makeLenses ''Viewport
makeLenses ''RenderState

instance IsString Widget where
    fromString = str

instance Default Result where
    def = Result V.emptyImage [] []

-- | Get the current rendering context.
getContext :: RenderM Context
getContext = ask

-- | When rendering the specified widget, use the specified border style
-- for any border rendering.
withBorderStyle :: BorderStyle -> Widget -> Widget
withBorderStyle bs p = Widget (hSize p) (vSize p) $ withReaderT (& activeBorderStyle .~ bs) (render p)

-- | Get the rendering context's active border style.
getActiveBorderStyle :: RenderM BorderStyle
getActiveBorderStyle = view activeBorderStyle

-- | The empty widget.
emptyWidget :: Widget
emptyWidget = raw V.emptyImage

renderFinal :: AttrMap
            -> [Widget]
            -> V.DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> RenderState
            -> (RenderState, V.Picture, Maybe CursorLocation)
renderFinal aMap layerRenders sz chooseCursor rs = (newRS, pic, theCursor)
    where
        (layerResults, newRS) = flip runState rs $ sequence $
            (\p -> runReaderT p ctx) <$>
            (render <$> cropToContext <$> layerRenders)
        ctx = Context def (fst sz) (snd sz) def aMap
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.image) <$> layerResults
        layerCursors = (^.cursors) <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

-- | Add an offset to all cursor locations and visbility requests
-- in the specified rendering result. This function is critical for
-- maintaining correctness in the rendering results as they are
-- processed successively by box layouts and other wrapping combinators,
-- since calls to this function result in converting from widget-local
-- coordinates to (ultimately) terminal-global ones so they can be used
-- by other combinators. You should call this any time you render
-- something and then translate it or otherwise offset it from its
-- original origin.
addResultOffset :: Location -> Result -> Result
addResultOffset off = addCursorOffset off . addVisibilityOffset off

addVisibilityOffset :: Location -> Result -> Result
addVisibilityOffset off r = r & visibilityRequests.each.vrPosition %~ (off <>)

addCursorOffset :: Location -> Result -> Result
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible loc = loc^.column >= 0 && loc^.row >= 0
    in r & cursors %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 100000

-- | The rendering context's current drawing attribute.
attr :: (Contravariant f, Functor f) => (V.Attr -> f V.Attr) -> Context -> f Context
attr = to (\c -> attrMapLookup (c^.ctxAttrName) (c^.ctxAttrs))

-- | Given an attribute name, obtain the attribute for the attribute
-- name by consulting the context's attribute map.
lookupAttrName :: AttrName -> RenderM V.Attr
lookupAttrName n = do
    c <- getContext
    return $ attrMapLookup n (c^.ctxAttrs)

-- | Build a widget from a 'String'. Breaks newlines up and space-pads
-- short lines out to the length of the longest line.
str :: String -> Widget
str s =
    Widget Fixed Fixed $ do
      c <- getContext
      let theLines = lines s
          fixEmpty [] = " "
          fixEmpty l = l
      case fixEmpty <$> theLines of
          -- The empty string case is important since we often need
          -- empty strings to have non-zero height!  This comes down to Vty's
          -- behavior of empty strings (they have imageHeight 1)
          [] -> return $ def & image .~ (V.string (c^.attr) "")
          [one] -> return $ def & image .~ (V.string (c^.attr) one)
          multiple ->
              let maxLength = maximum $ length <$> multiple
                  lineImgs = lineImg <$> multiple
                  lineImg lStr = V.string (c^.attr) (lStr ++ replicate (maxLength - length lStr) ' ')
              in return $ def & image .~ (V.vertCat lineImgs)

-- | Build a widget from a 'T.Text' value.  Behaves the same as 'str'.
txt :: T.Text -> Widget
txt = str . T.unpack

-- | Pad the specified widget on the left. Grows horizontally but defers
-- vertical growth to the padded widget.
padLeft :: Widget -> Widget
padLeft p =
    Widget Unlimited (vSize p) $ do
        result <- render p
        render $ (vLimit (result^.image.to V.imageHeight) $ fill ' ') <+> (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the right. Grows horizontally but
-- defers vertical growth to the padded widget.
padRight :: Widget -> Widget
padRight p =
    Widget Unlimited (vSize p) $ do
        result <- render p
        render $ (Widget Fixed Fixed $ return result) <+> (vLimit (result^.image.to V.imageHeight) $ fill ' ')

-- | Pad the specified widget on the top. Grows vertically but defers
-- horizontal growth to the padded widget.
padTop :: Widget -> Widget
padTop p =
    Widget (hSize p) Unlimited $ do
        result <- render p
        render $ (hLimit (result^.image.to V.imageWidth) $ fill ' ') <=> (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the bottom. Grows vertically but defers
-- horizontal growth to the padded widget.
padBottom :: Widget -> Widget
padBottom p =
    Widget (hSize p) Unlimited $ do
        result <- render p
        render $ (Widget Fixed Fixed $ return result) <=> (hLimit (result^.image.to V.imageWidth) $ fill ' ')

-- | Fill all available space with the specified character. Grows both
-- horizontally and vertically.
fill :: Char -> Widget
fill ch =
    Widget Unlimited Unlimited $ do
      c <- getContext
      return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (c^.availH))

-- | Vertical box layout: put the specified widgets one above the other
-- in the specified order (uppermost first). Defers growth policies to
-- the growth policies of both widgets.
vBox :: [Widget] -> Widget
vBox [] = emptyWidget
vBox pairs = renderBox vBoxRenderer pairs

-- | Horizontal box layout: put the specified widgets next to each other
-- in the specified order (leftmost first). Defers growth policies to
-- the growth policies of both widgets.
hBox :: [Widget] -> Widget
hBox [] = emptyWidget
hBox pairs = renderBox hBoxRenderer pairs

data BoxRenderer =
    BoxRenderer { contextPrimary :: Lens' Context Int
                , contextSecondary :: Lens' Context Int
                , imagePrimary :: V.Image -> Int
                , limitPrimary :: Int -> Widget -> Widget
                , limitSecondary :: Int -> Widget -> Widget
                , primarySize :: Widget -> Size
                , concatenate :: [V.Image] -> V.Image
                , locationFromOffset :: Int -> Location
                }

vBoxRenderer :: BoxRenderer
vBoxRenderer = BoxRenderer availH availW V.imageHeight vLimit hLimit vSize V.vertCat (Location . (0 ,))

hBoxRenderer :: BoxRenderer
hBoxRenderer = BoxRenderer availW availH V.imageWidth hLimit vLimit hSize V.horizCat (Location . (, 0))

renderBox :: BoxRenderer -> [Widget] -> Widget
renderBox br ws = do
    Widget (maximum $ hSize <$> ws) (maximum $ vSize <$> ws) $ do
      c <- getContext

      let pairsIndexed = zip [(0::Int)..] ws
          (his, lows) = partition (\p -> (primarySize br $ snd p) == Fixed) pairsIndexed

      renderedHis <- mapM (\(i, prim) -> (i,) <$> render prim) his

      renderedLows <- case lows of
          [] -> return []
          ls -> do
              let remainingPrimary = c^.(contextPrimary br) - (sum $ (^._2.image.(to $ imagePrimary br)) <$> renderedHis)
                  primaryPerLow = remainingPrimary `div` length ls
                  padFirst = remainingPrimary - (primaryPerLow * length ls)
                  secondaryPerLow = c^.(contextSecondary br)
                  primaries = replicate (length ls) primaryPerLow & ix 0 %~ (+ padFirst)

              let renderLow ((i, prim), pri) =
                      (i,) <$> (render $ limitPrimary br pri
                                       $ limitSecondary br secondaryPerLow
                                       $ cropToContext prim)

              if remainingPrimary > 0 then mapM renderLow (zip ls primaries) else return []

      let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
          allResults = snd <$> rendered
          allImages = (^.image) <$> allResults
          allPrimaries = imagePrimary br <$> allImages
          allTranslatedResults = (flip map) (zip [0..] allResults) $ \(i, result) ->
              let off = locationFromOffset br offPrimary
                  offPrimary = sum $ take i allPrimaries
              in addResultOffset off result

      cropResultToContext $ Result (concatenate br allImages)
                            (concat $ _cursors <$> allTranslatedResults)
                            (concat $ _visibilityRequests <$> allTranslatedResults)

-- | Limit the space available to the specified widget to the specified
-- number of columns. This is important for constraining the horizontal
-- growth of otherwise-unlimited widgets.
hLimit :: Int -> Widget -> Widget
hLimit w p =
    Widget Fixed (vSize p) $ do
      withReaderT (& availW .~ w) $ render $ cropToContext p

-- | Limit the space available to the specified widget to the specified
-- number of rows. This is important for constraining the vertical
-- growth of otherwise-unlimited widgets.
vLimit :: Int -> Widget -> Widget
vLimit h p =
    Widget (hSize p) Fixed $ do
      withReaderT (& availH .~ h) $ render $ cropToContext p

-- | When drawing the specified widget, set the current attribute used
-- for drawing to the one with the specified name. Note that the widget
-- may use further calls to 'withAttr' to override this; if you really
-- want to prevent that, use 'withDefaultAttr' or 'forceAttr'.
withAttr :: AttrName -> Widget -> Widget
withAttr an p =
    Widget (hSize p) (vSize p) $ do
      withReaderT (& ctxAttrName .~ an) (render p)

-- | Update the attribute map while rendering the specified widget: set
-- its new default attribute to the one that we get by looking up the
-- specified attribute name in the map.
withDefaultAttr :: AttrName -> Widget -> Widget
withDefaultAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrs %~ (setDefault (attrMapLookup an (c^.ctxAttrs)))) (render p)

-- | When rendering the specified widget, update the attribute map with
-- the specified transformation.
updateAttrMap :: (AttrMap -> AttrMap) -> Widget -> Widget
updateAttrMap f p =
    Widget (hSize p) (vSize p) $ do
        withReaderT (& ctxAttrs %~ f) (render p)

-- | When rendering the specified widget, force all attribute lookups
-- in the attribute map to use the value currently assigned to the
-- specified attribute name.
forceAttr :: AttrName -> Widget -> Widget
forceAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrs .~ (forceAttrMap (attrMapLookup an (c^.ctxAttrs)))) (render p)

-- | Build a widget directly from a raw Vty image.
raw :: V.Image -> Widget
raw img = Widget Fixed Fixed $ return $ def & image .~ img

-- | Translate the specified widget by the specified offset amount.
translateBy :: Location -> Widget -> Widget
translateBy loc p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ addResultOffset loc
             $ result & image %~ (V.translate (loc^.column) (loc^.row))

cropResultToContext :: Result -> RenderM Result
cropResultToContext result = do
    c <- getContext
    return $ result & image %~ (V.crop (c^.availW) (c^.availH))

cropToContext :: Widget -> Widget
cropToContext p =
    Widget (hSize p) (vSize p) $ (render p >>= cropResultToContext)

-- | Crop the specified widget on the left by the specified number of
-- columns.
cropLeftBy :: Int -> Widget -> Widget
cropLeftBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.image) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
      return $ addResultOffset (Location (-1 * cols, 0))
             $ result & image %~ cropped

-- | Crop the specified widget on the right by the specified number of
-- columns.
cropRightBy :: Int -> Widget -> Widget
cropRightBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.image) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
      return $ result & image %~ cropped

-- | Crop the specified widget on the top by the specified number of
-- rows.
cropTopBy :: Int -> Widget -> Widget
cropTopBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.image) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
      return $ addResultOffset (Location (0, -1 * rows))
             $ result & image %~ cropped

-- | Crop the specified widget on the bottom by the specified number of
-- rows.
cropBottomBy :: Int -> Widget -> Widget
cropBottomBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.image) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
      return $ result & image %~ cropped

-- | When rendering the specified widget, also register a cursor
-- positioning request using the specified name and location.
showCursor :: Name -> Location -> Widget -> Widget
showCursor n cloc p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ result & cursors %~ (CursorLocation cloc (Just n):)

hRelease :: Widget -> Maybe Widget
hRelease p =
    case hSize p of
        Fixed -> Just $ Widget Unlimited (vSize p) $ withReaderT (& availW .~ unrestricted) (render p)
        Unlimited -> Nothing

vRelease :: Widget -> Maybe Widget
vRelease p =
    case vSize p of
        Fixed -> Just $ Widget (hSize p) Unlimited $ withReaderT (& availH .~ unrestricted) (render p)
        Unlimited -> Nothing

-- | Render the specified widget in a named viewport with the
-- specified type. This permits widgets to be scrolled without being
-- scrolling-aware. To make the most use of viewports, the specified
-- widget should use the 'visible' combinator to make a "visibility
-- request". This viewport combinator will then translate the resulting
-- rendering to make the requested region visible. In addition, the
-- 'Brick.Main.EventM' monad provides primitives to scroll viewports
-- created by this function if 'visible' is not what you want.
--
-- If a viewport receives more than one visibility request, only the
-- first is honored. If a viewport receives more than one scrolling
-- request from 'Brick.Main.EventM', all are honored in the order in
-- which they are received.
viewport :: Name
         -- ^ The name of the viewport (must be unique and stable for
         -- reliable behavior)
         -> ViewportType
         -- ^ The type of viewport (indicates the permitted scrolling
         -- direction)
         -> Widget
         -- ^ The widget to be rendered in the scrollable viewport
         -> Widget
viewport vpname typ p =
    Widget Unlimited Unlimited $ do
      -- First, update the viewport size.
      c <- getContext
      let newVp = VP 0 0 newSize
          newSize = (c^.availW, c^.availH)
          doInsert (Just vp) = Just $ vp & vpSize .~ newSize
          doInsert Nothing = Just newVp

      lift $ modify (& viewportMap %~ (M.alter doInsert vpname))

      -- Then render the sub-rendering with the rendering layout
      -- constraint released (but raise an exception if we are asked to
      -- render an infinitely-sized widget in the viewport's scrolling
      -- dimension)
      let Name vpn = vpname
          release = case typ of
            Vertical -> vRelease
            Horizontal -> hRelease
          released = case release p of
            Just w -> w
            Nothing -> case typ of
                Vertical -> error $ "tried to embed an infinite-height widget in vertical viewport " <> (show vpn)
                Horizontal -> error $ "tried to embed an infinite-width widget in horizontal viewport " <> (show vpn)

      initialResult <- render released

      -- If the sub-rendering requested visibility, update the scroll
      -- state accordingly
      when (not $ null $ initialResult^.visibilityRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMap.to (M.lookup vpname))
          let rq = head $ initialResult^.visibilityRequests
              updatedVp = scrollToView typ rq vp
          lift $ modify (& viewportMap %~ (M.insert vpname updatedVp))

      -- If the rendering state includes any scrolling requests for this
      -- viewport, apply those
      reqs <- lift $ gets $ (^.scrollRequests)
      let relevantRequests = snd <$> filter (\(n, _) -> n == vpname) reqs
      when (not $ null relevantRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMap.to (M.lookup vpname))
          let updatedVp = applyRequests relevantRequests vp
              applyRequests [] v = v
              applyRequests (rq:rqs) v = scrollTo typ rq (initialResult^.image) $ applyRequests rqs v
          lift $ modify (& viewportMap %~ (M.insert vpname updatedVp))
          return ()

      -- Get the viewport state now that it has been updated.
      Just vp <- lift $ gets (M.lookup vpname . (^.viewportMap))

      -- Then perform a translation of the sub-rendering to fit into the
      -- viewport
      translated <- render $ translateBy (Location (-1 * vp^.vpLeft, -1 * vp^.vpTop))
                           $ Widget Fixed Fixed $ return initialResult

      -- Return the translated result with the visibility requests
      -- discarded
      let translatedSize = ( translated^.image.to V.imageWidth
                           , translated^.image.to V.imageHeight
                           )
      case translatedSize of
          (0, 0) -> return $ translated & image .~ (V.charFill (c^.attr) ' ' (c^.availW) (c^.availH))
                                        & visibilityRequests .~ mempty
          _ -> render $ cropToContext
                      $ padBottom
                      $ padRight
                      $ Widget Fixed Fixed $ return $ translated & visibilityRequests .~ mempty

scrollTo :: ViewportType -> ScrollRequest -> V.Image -> Viewport -> Viewport
scrollTo typ req img vp = vp & theStart .~ newStart
    where
        theStart :: Lens' Viewport Int
        theStart = case typ of
            Horizontal -> vpLeft
            Vertical -> vpTop
        theSize = case typ of
            Horizontal -> vpSize._1
            Vertical -> vpSize._2
        imgSize = case typ of
            Horizontal -> V.imageWidth img
            Vertical -> V.imageHeight img

        newStart = clamp 0 (imgSize - vp^.theSize) adjustedAmt
        adjustedAmt = case req of
            ScrollBy amt -> (vp^.theStart) + amt
            ScrollPage Up -> (vp^.theStart) - (vp^.theSize)
            ScrollPage Down -> (vp^.theStart) + (vp^.theSize)
            ScrollToBeginning -> 0
            ScrollToEnd -> imgSize - (vp^.theSize)

scrollToView :: ViewportType -> VisibilityRequest -> Viewport -> Viewport
scrollToView typ rq vp = vp & theStart .~ newStart
    where
        theStart :: Lens' Viewport Int
        theStart = case typ of
            Horizontal -> vpLeft
            Vertical -> vpTop
        theSize = case typ of
            Horizontal -> vpSize._1
            Vertical -> vpSize._2
        reqStart = case typ of
            Horizontal -> rq^.vrPosition.column
            Vertical -> rq^.vrPosition.row
        reqSize = case typ of
            Horizontal -> rq^.vrSize._1
            Vertical -> rq^.vrSize._2

        curStart = vp^.theStart
        curEnd = curStart + vp^.theSize

        reqEnd = reqStart + reqSize
        newStart :: Int
        newStart = if reqStart < curStart
                   then reqStart
                   else if reqStart > curEnd || reqEnd > curEnd
                        then reqEnd - vp^.theSize
                        else curStart

-- | Request that the specified widget be made visible when it is
-- rendered inside a viewport. This permits widgets (whose sizes and
-- positions cannot be known due to being embedded in arbitrary layouts)
-- make a request for a parent viewport to locate them and scroll enough
-- to put them in view. This, together with 'viewport' is what makes the
-- text editor and list widgets possible without making them deal with
-- the details of scrolling state management.
visible :: Widget -> Widget
visible p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let imageSize = ( result^.image.to V.imageWidth
                      , result^.image.to V.imageHeight
                      )
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if imageSize^._1 > 0 && imageSize^._2 > 0
               then result & visibilityRequests %~ (VR (Location (0, 0)) imageSize :)
               else result

-- | Similar to 'visible', request that a region (with the specified
-- 'Location' as its origin and 'V.DisplayRegion' as its size) be made
-- visible when it is rendered inside a viewport.
visibleRegion :: Location -> V.DisplayRegion -> Widget -> Widget
visibleRegion vrloc sz p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if sz^._1 > 0 && sz^._2 > 0
               then result & visibilityRequests %~ (VR vrloc sz :)
               else result

-- | Horizontal box layout: put the specified widgets next to each other
-- in the specified order. Defers growth policies to the growth policies
-- of both widgets.
(<+>) :: Widget
      -- ^ Left
      -> Widget
      -- ^ Right
      -> Widget
(<+>) a b = hBox [a, b]

-- | Vertical box layout: put the specified widgets one above the other
-- in the specified order. Defers growth policies to the growth policies
-- of both widgets.
(<=>) :: Widget
      -- ^ Top
      -> Widget
      -- ^ Bottom
      -> Widget
(<=>) a b = vBox [a, b]
