{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.Internal
  ( Result(..)
  , visibilityRequestsL
  , imageL
  , cursorsL
  , addResultOffset

  , VisibilityRequest(..)
  , vrPositionL
  , vrSizeL

  , RenderState(..)
  , ScrollRequest(..)
  , Direction(..)

  , renderFinal
  , Widget(..)
  , Size(..)
  , RenderM

  , Context(ctxAttrName, availWidth, availHeight, ctxBorderStyle, ctxAttrMap)
  , lookupAttrName
  , getContext
  , attrL
  , availWidthL
  , availHeightL
  , ctxAttrMapL
  , ctxAttrNameL
  , ctxBorderStyleL

  , withBorderStyle

  , ViewportType(..)

  , txt
  , str
  , fill

  , Padding(..)
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padLeftRight
  , padTopBottom
  , padAll

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
import Control.Lens (makeLenses, (^.), (.~), (&), (%~), to, _1, _2, each, to, ix)
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
    VR { vrPosition :: Location
       , vrSize :: V.DisplayRegion
       }
       deriving Show

-- | The type of viewports that indicates the direction(s) in which a
-- viewport is scrollable.
data ViewportType = Vertical
                  -- ^ Viewports of this type are scrollable only vertically.
                  | Horizontal
                  -- ^ Viewports of this type are scrollable only horizontally.
                  | Both
                  -- ^ Viewports of this type are scrollable vertically and horizontally.
                  deriving Show

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
    Result { image :: V.Image
           -- ^ The final rendered image for a widget
           , cursors :: [CursorLocation]
           -- ^ The list of reported cursor positions for the
           -- application to choose from
           , visibilityRequests :: [VisibilityRequest]
           -- ^ The list of visibility requests made by widgets rendered
           -- while rendering this one (used by viewports)
           }
           deriving Show

-- | The rendering context. This tells widgets how to render: how much
-- space they have in which to render, which attribute they should use
-- to render, which bordring style should be used, and the attribute map
-- available for rendering.
data Context =
    Context { ctxAttrName :: AttrName
            , availWidth :: Int
            , availHeight :: Int
            , ctxBorderStyle :: BorderStyle
            , ctxAttrMap :: AttrMap
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

-- | Scrolling direction.
data Direction = Up
               -- ^ Up/left
               | Down
               -- ^ Down/right

data ScrollRequest = HScrollBy Int
                   | HScrollPage Direction
                   | HScrollToBeginning
                   | HScrollToEnd
                   | VScrollBy Int
                   | VScrollPage Direction
                   | VScrollToBeginning
                   | VScrollToEnd

data RenderState =
    RS { viewportMap :: M.Map Name Viewport
       , scrollRequests :: [(Name, ScrollRequest)]
       }

suffixLenses ''Result
suffixLenses ''Context
suffixLenses ''VisibilityRequest
suffixLenses ''RenderState

makeLenses ''Viewport

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
withBorderStyle bs p = Widget (hSize p) (vSize p) $ withReaderT (& ctxBorderStyleL .~ bs) (render p)

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
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.imageL) <$> layerResults
        layerCursors = (^.cursorsL) <$> layerResults
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
addVisibilityOffset off r = r & visibilityRequestsL.each.vrPositionL %~ (off <>)

addCursorOffset :: Location -> Result -> Result
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible l = l^.columnL >= 0 && l^.rowL >= 0
    in r & cursorsL %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 100000

-- | The rendering context's current drawing attribute.
attrL :: (Contravariant f, Functor f) => (V.Attr -> f V.Attr) -> Context -> f Context
attrL = to (\c -> attrMapLookup (c^.ctxAttrNameL) (c^.ctxAttrMapL))

-- | Given an attribute name, obtain the attribute for the attribute
-- name by consulting the context's attribute map.
lookupAttrName :: AttrName -> RenderM V.Attr
lookupAttrName n = do
    c <- getContext
    return $ attrMapLookup n (c^.ctxAttrMapL)

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
          [] -> return $ def & imageL .~ (V.string (c^.attrL) "")
          [one] -> return $ def & imageL .~ (V.string (c^.attrL) one)
          multiple ->
              let maxLength = maximum $ length <$> multiple
                  lineImgs = lineImg <$> multiple
                  lineImg lStr = V.string (c^.attrL) (lStr ++ replicate (maxLength - length lStr) ' ')
              in return $ def & imageL .~ (V.vertCat lineImgs)

-- | Build a widget from a 'T.Text' value.  Behaves the same as 'str'.
txt :: T.Text -> Widget
txt = str . T.unpack

-- | The type of padding.
data Padding = Pad Int
             -- ^ Pad by the specified number of rows or columns.
             | Max
             -- ^ Pad up to the number of available rows or columns.

-- | Pad the specified widget on the left.
padLeft :: Padding -> Widget -> Widget
padLeft padding p =
    let (f, sz) = case padding of
          Max -> (id, Unlimited)
          Pad i -> (hLimit i, hSize p)
    in Widget sz (vSize p) $ do
        result <- render p
        render $ (f $ vLimit (result^.imageL.to V.imageHeight) $ fill ' ') <+>
                 (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the right.
padRight :: Padding -> Widget -> Widget
padRight padding p =
    let (f, sz) = case padding of
          Max -> (id, Unlimited)
          Pad i -> (hLimit i, hSize p)
    in Widget sz (vSize p) $ do
        result <- render p
        render $ (Widget Fixed Fixed $ return result) <+>
                 (f $ vLimit (result^.imageL.to V.imageHeight) $ fill ' ')

-- | Pad the specified widget on the top.
padTop :: Padding -> Widget -> Widget
padTop padding p =
    let (f, sz) = case padding of
          Max -> (id, Unlimited)
          Pad i -> (vLimit i, vSize p)
    in Widget (hSize p) sz $ do
        result <- render p
        render $ (f $ hLimit (result^.imageL.to V.imageWidth) $ fill ' ') <=>
                 (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the bottom.
padBottom :: Padding -> Widget -> Widget
padBottom padding p =
    let (f, sz) = case padding of
          Max -> (id, Unlimited)
          Pad i -> (vLimit i, vSize p)
    in Widget (hSize p) sz $ do
        result <- render p
        render $ (Widget Fixed Fixed $ return result) <=>
                 (f $ hLimit (result^.imageL.to V.imageWidth) $ fill ' ')

-- | Pad a widget on the left and right.
padLeftRight :: Int -> Widget -> Widget
padLeftRight c w = padLeft (Pad c) $ padRight (Pad c) w

-- | Pad a widget on the top and bottom.
padTopBottom :: Int -> Widget -> Widget
padTopBottom r w = padTop (Pad r) $ padBottom (Pad r) w

-- | Pad a widget on all sides.
padAll :: Int -> Widget -> Widget
padAll v w = padLeftRight v $ padTopBottom v w

-- | Fill all available space with the specified character. Grows both
-- horizontally and vertically.
fill :: Char -> Widget
fill ch =
    Widget Unlimited Unlimited $ do
      c <- getContext
      return $ def & imageL .~ (V.charFill (c^.attrL) ch (c^.availWidthL) (c^.availHeightL))

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
                , imageSecondary :: V.Image -> Int
                , limitPrimary :: Int -> Widget -> Widget
                , limitSecondary :: Int -> Widget -> Widget
                , primarySize :: Widget -> Size
                , concatenatePrimary :: [V.Image] -> V.Image
                , locationFromOffset :: Int -> Location
                , padImageSecondary :: Int -> V.Image -> V.Attr -> V.Image
                }

vBoxRenderer :: BoxRenderer
vBoxRenderer =
    BoxRenderer availHeightL availWidthL V.imageHeight V.imageWidth vLimit hLimit vSize V.vertCat (Location . (0 ,))
                (\amt img a -> let p = V.charFill a ' ' amt (V.imageHeight img)
                               in V.horizCat [img, p])

hBoxRenderer :: BoxRenderer
hBoxRenderer =
    BoxRenderer availWidthL availHeightL V.imageWidth V.imageHeight hLimit vLimit hSize V.horizCat (Location . (, 0))
                (\amt img a -> let p = V.charFill a ' ' (V.imageWidth img) amt
                               in V.vertCat [img, p])

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
              let remainingPrimary = c^.(contextPrimary br) - (sum $ (^._2.imageL.(to $ imagePrimary br)) <$> renderedHis)
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
          allImages = (^.imageL) <$> allResults
          allPrimaries = imagePrimary br <$> allImages
          allTranslatedResults = (flip map) (zip [0..] allResults) $ \(i, result) ->
              let off = locationFromOffset br offPrimary
                  offPrimary = sum $ take i allPrimaries
              in addResultOffset off result
          -- Determine the secondary dimension value to pad to. In a
          -- vertical box we want all images to be the same width to
          -- avoid attribute over-runs or blank spaces with the wrong
          -- attribute. In a horizontal box we want all images to have
          -- the same height for the same reason.
          maxSecondary = maximum $ imageSecondary br <$> allImages
          padImage img = padImageSecondary br (maxSecondary - imageSecondary br img) img (c^.attrL)
          paddedImages = padImage <$> allImages

      cropResultToContext $ Result (concatenatePrimary br paddedImages)
                            (concat $ cursors <$> allTranslatedResults)
                            (concat $ visibilityRequests <$> allTranslatedResults)

-- | Limit the space available to the specified widget to the specified
-- number of columns. This is important for constraining the horizontal
-- growth of otherwise-unlimited widgets.
hLimit :: Int -> Widget -> Widget
hLimit w p =
    Widget Fixed (vSize p) $ do
      withReaderT (& availWidthL .~ w) $ render $ cropToContext p

-- | Limit the space available to the specified widget to the specified
-- number of rows. This is important for constraining the vertical
-- growth of otherwise-unlimited widgets.
vLimit :: Int -> Widget -> Widget
vLimit h p =
    Widget (hSize p) Fixed $ do
      withReaderT (& availHeightL .~ h) $ render $ cropToContext p

-- | When drawing the specified widget, set the current attribute used
-- for drawing to the one with the specified name. Note that the widget
-- may use further calls to 'withAttr' to override this; if you really
-- want to prevent that, use 'withDefaultAttr' or 'forceAttr'.
withAttr :: AttrName -> Widget -> Widget
withAttr an p =
    Widget (hSize p) (vSize p) $ do
      withReaderT (& ctxAttrNameL .~ an) (render p)

-- | Update the attribute map while rendering the specified widget: set
-- its new default attribute to the one that we get by looking up the
-- specified attribute name in the map.
withDefaultAttr :: AttrName -> Widget -> Widget
withDefaultAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrMapL %~ (setDefault (attrMapLookup an (c^.ctxAttrMapL)))) (render p)

-- | When rendering the specified widget, update the attribute map with
-- the specified transformation.
updateAttrMap :: (AttrMap -> AttrMap) -> Widget -> Widget
updateAttrMap f p =
    Widget (hSize p) (vSize p) $ do
        withReaderT (& ctxAttrMapL %~ f) (render p)

-- | When rendering the specified widget, force all attribute lookups
-- in the attribute map to use the value currently assigned to the
-- specified attribute name.
forceAttr :: AttrName -> Widget -> Widget
forceAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrMapL .~ (forceAttrMap (attrMapLookup an (c^.ctxAttrMapL)))) (render p)

-- | Build a widget directly from a raw Vty image.
raw :: V.Image -> Widget
raw img = Widget Fixed Fixed $ return $ def & imageL .~ img

-- | Translate the specified widget by the specified offset amount.
translateBy :: Location -> Widget -> Widget
translateBy off p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ addResultOffset off
             $ result & imageL %~ (V.translate (off^.columnL) (off^.rowL))

cropResultToContext :: Result -> RenderM Result
cropResultToContext result = do
    c <- getContext
    return $ result & imageL %~ (V.crop (c^.availWidthL) (c^.availHeightL))

cropToContext :: Widget -> Widget
cropToContext p =
    Widget (hSize p) (vSize p) $ (render p >>= cropResultToContext)

-- | Crop the specified widget on the left by the specified number of
-- columns.
cropLeftBy :: Int -> Widget -> Widget
cropLeftBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.imageL) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
      return $ addResultOffset (Location (-1 * cols, 0))
             $ result & imageL %~ cropped

-- | Crop the specified widget on the right by the specified number of
-- columns.
cropRightBy :: Int -> Widget -> Widget
cropRightBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.imageL) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
      return $ result & imageL %~ cropped

-- | Crop the specified widget on the top by the specified number of
-- rows.
cropTopBy :: Int -> Widget -> Widget
cropTopBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.imageL) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
      return $ addResultOffset (Location (0, -1 * rows))
             $ result & imageL %~ cropped

-- | Crop the specified widget on the bottom by the specified number of
-- rows.
cropBottomBy :: Int -> Widget -> Widget
cropBottomBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.imageL) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
      return $ result & imageL %~ cropped

-- | When rendering the specified widget, also register a cursor
-- positioning request using the specified name and location.
showCursor :: Name -> Location -> Widget -> Widget
showCursor n cloc p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ result & cursorsL %~ (CursorLocation cloc (Just n):)

hRelease :: Widget -> Maybe Widget
hRelease p =
    case hSize p of
        Fixed -> Just $ Widget Unlimited (vSize p) $ withReaderT (& availWidthL .~ unrestricted) (render p)
        Unlimited -> Nothing

vRelease :: Widget -> Maybe Widget
vRelease p =
    case vSize p of
        Fixed -> Just $ Widget (hSize p) Unlimited $ withReaderT (& availHeightL .~ unrestricted) (render p)
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
          newSize = (c^.availWidthL, c^.availHeightL)
          doInsert (Just vp) = Just $ vp & vpSize .~ newSize
          doInsert Nothing = Just newVp

      lift $ modify (& viewportMapL %~ (M.alter doInsert vpname))

      -- Then render the sub-rendering with the rendering layout
      -- constraint released (but raise an exception if we are asked to
      -- render an infinitely-sized widget in the viewport's scrolling
      -- dimension)
      let Name vpn = vpname
          release = case typ of
            Vertical -> vRelease
            Horizontal -> hRelease
            Both -> \w -> vRelease w >>= hRelease
          released = case release p of
            Just w -> w
            Nothing -> case typ of
                Vertical -> error $ "tried to embed an infinite-height widget in vertical viewport " <> (show vpn)
                Horizontal -> error $ "tried to embed an infinite-width widget in horizontal viewport " <> (show vpn)
                Both -> error $ "tried to embed an infinite-width or infinite-height widget in 'Both' type viewport " <> (show vpn)

      initialResult <- render released

      -- If the sub-rendering requested visibility, update the scroll
      -- state accordingly
      when (not $ null $ initialResult^.visibilityRequestsL) $ do
          Just vp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
          let rq = head $ initialResult^.visibilityRequestsL
              updatedVp = case typ of
                  Both -> scrollToView Horizontal rq $ scrollToView Vertical rq vp
                  Horizontal -> scrollToView typ rq vp
                  Vertical -> scrollToView typ rq vp
          lift $ modify (& viewportMapL %~ (M.insert vpname updatedVp))

      -- If the rendering state includes any scrolling requests for this
      -- viewport, apply those
      reqs <- lift $ gets $ (^.scrollRequestsL)
      let relevantRequests = snd <$> filter (\(n, _) -> n == vpname) reqs
      when (not $ null relevantRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
          let updatedVp = applyRequests relevantRequests vp
              applyRequests [] v = v
              applyRequests (rq:rqs) v =
                  case typ of
                      Horizontal -> scrollTo typ rq (initialResult^.imageL) $ applyRequests rqs v
                      Vertical -> scrollTo typ rq (initialResult^.imageL) $ applyRequests rqs v
                      Both -> scrollTo Horizontal rq (initialResult^.imageL) $
                              scrollTo Vertical rq (initialResult^.imageL) $
                              applyRequests rqs v
          lift $ modify (& viewportMapL %~ (M.insert vpname updatedVp))
          return ()

      -- Get the viewport state now that it has been updated.
      Just vp <- lift $ gets (M.lookup vpname . (^.viewportMapL))

      -- Then perform a translation of the sub-rendering to fit into the
      -- viewport
      translated <- render $ translateBy (Location (-1 * vp^.vpLeft, -1 * vp^.vpTop))
                           $ Widget Fixed Fixed $ return initialResult

      -- Return the translated result with the visibility requests
      -- discarded
      let translatedSize = ( translated^.imageL.to V.imageWidth
                           , translated^.imageL.to V.imageHeight
                           )
      case translatedSize of
          (0, 0) -> return $ translated & imageL .~ (V.charFill (c^.attrL) ' ' (c^.availWidthL) (c^.availHeightL))
                                        & visibilityRequestsL .~ mempty
          _ -> render $ cropToContext
                      $ padBottom Max
                      $ padRight Max
                      $ Widget Fixed Fixed $ return $ translated & visibilityRequestsL .~ mempty

scrollTo :: ViewportType -> ScrollRequest -> V.Image -> Viewport -> Viewport
scrollTo Both _ _ _ = error "BUG: called scrollTo on viewport type 'Both'"
scrollTo Vertical req img vp = vp & vpTop .~ newVStart
    where
        newVStart = clamp 0 (V.imageHeight img - vp^.vpSize._2) adjustedAmt
        adjustedAmt = case req of
            VScrollBy amt -> vp^.vpTop + amt
            VScrollPage Up -> vp^.vpTop - vp^.vpSize._2
            VScrollPage Down -> vp^.vpTop + vp^.vpSize._2
            VScrollToBeginning -> 0
            VScrollToEnd -> V.imageHeight img - vp^.vpSize._2
            _ -> vp^.vpTop
scrollTo Horizontal req img vp = vp & vpLeft .~ newHStart
    where
        newHStart = clamp 0 (V.imageWidth img - vp^.vpSize._1) adjustedAmt
        adjustedAmt = case req of
            HScrollBy amt -> vp^.vpLeft + amt
            HScrollPage Up -> vp^.vpLeft - vp^.vpSize._1
            HScrollPage Down -> vp^.vpLeft + vp^.vpSize._1
            HScrollToBeginning -> 0
            HScrollToEnd -> V.imageWidth img - vp^.vpSize._1
            _ -> vp^.vpLeft

scrollToView :: ViewportType -> VisibilityRequest -> Viewport -> Viewport
scrollToView Both _ _ = error "BUG: called scrollToView on 'Both' type viewport"
scrollToView Vertical rq vp = vp & vpTop .~ newVStart
    where
        curStart = vp^.vpTop
        curEnd = curStart + vp^.vpSize._2
        reqStart = rq^.vrPositionL.rowL

        reqEnd = rq^.vrPositionL.rowL + rq^.vrSizeL._2
        newVStart :: Int
        newVStart = if reqStart < curStart
                   then reqStart
                   else if reqStart > curEnd || reqEnd > curEnd
                        then reqEnd - vp^.vpSize._2
                        else curStart
scrollToView Horizontal rq vp = vp & vpLeft .~ newHStart
    where
        curStart = vp^.vpLeft
        curEnd = curStart + vp^.vpSize._1
        reqStart = rq^.vrPositionL.columnL

        reqEnd = rq^.vrPositionL.columnL + rq^.vrSizeL._1
        newHStart :: Int
        newHStart = if reqStart < curStart
                   then reqStart
                   else if reqStart > curEnd || reqEnd > curEnd
                        then reqEnd - vp^.vpSize._1
                        else curStart

-- | Request that the specified widget be made visible when it is
-- rendered inside a viewport. This permits widgets (whose sizes and
-- positions cannot be known due to being embedded in arbitrary layouts)
-- to make a request for a parent viewport to locate them and scroll
-- tenough o put them in view. This, together with 'viewport' is what
-- tmakes the ext editor and list widgets possible without making them
-- tdeal with he details of scrolling state management.
visible :: Widget -> Widget
visible p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let imageSize = ( result^.imageL.to V.imageWidth
                      , result^.imageL.to V.imageHeight
                      )
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if imageSize^._1 > 0 && imageSize^._2 > 0
               then result & visibilityRequestsL %~ (VR (Location (0, 0)) imageSize :)
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
               then result & visibilityRequestsL %~ (VR vrloc sz :)
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
