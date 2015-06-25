{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Render.Internal
  ( Result(..)
  , image
  , cursors
  , attr
  , ctxAttrs
  , lookupAttrName
  , visibilityRequests

  , RenderState(..)
  , ScrollRequest(..)
  , Direction(..)

  , Priority(..)
  , renderFinal
  , Widget(..)
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
  , hPad
  , vPad
  , hFill
  , vFill

  , hBox
  , vBox
  , (=>>), (<<=), (<=>)
  , (+>>), (<<+), (<+>)

  , hLimit
  , vLimit
  , withDefaultAttr
  , withAttrName
  , withAttrMappings
  , forceAttr
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
import Control.Lens (makeLenses, (^.), (.~), (&), (%~), to, _1, _2, view, each, to)
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
import Data.List (sortBy)
import Control.Lens (Lens')
import Data.String (IsString(..))
import qualified Graphics.Vty as V

import Brick.Core
import Brick.Border.Style
import Brick.Util (clOffset, for)
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

data Result =
    Result { _image :: V.Image
           , _cursors :: [CursorLocation]
           , _visibilityRequests :: [VisibilityRequest]
           }
           deriving Show

data Context =
    Context { _ctxAttrName :: AttrName
            , _availW :: Int
            , _availH :: Int
            , _activeBorderStyle :: BorderStyle
            , _ctxAttrs :: AttrMap
            }

data Priority = High | Low
              deriving (Show, Eq)

type RenderM a = ReaderT Context (State RenderState) a

data Widget =
    Widget { render :: RenderM Result
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

getContext :: RenderM Context
getContext = ask

withBorderStyle :: BorderStyle -> Widget -> Widget
withBorderStyle bs p = Widget $ withReaderT (& activeBorderStyle .~ bs) (render p)

getActiveBorderStyle :: RenderM BorderStyle
getActiveBorderStyle = view activeBorderStyle

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

addVisibilityOffset :: Location -> Result -> Result
addVisibilityOffset off r = r & visibilityRequests.each.vrPosition %~ (off <>)

addCursorOffset :: Location -> Result -> Result
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible (CursorLocation (Location (width, height)) _) = width >= 0 && height >= 0
    in r & cursors %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 100000

attr :: (Contravariant f, Functor f) => (V.Attr -> f V.Attr) -> Context -> f Context
attr = to (\c -> attrMapLookup (c^.ctxAttrName) (c^.ctxAttrs))

lookupAttrName :: AttrName -> RenderM V.Attr
lookupAttrName n = do
    c <- getContext
    return $ attrMapLookup n (c^.ctxAttrs)

str :: String -> Widget
str s =
    Widget $ do
      c <- getContext
      return $ def & image .~ (V.string (c^.attr) s)

txt :: T.Text -> Widget
txt = str . T.unpack

hPad :: Char -> Widget
hPad ch =
    Widget $ do
      c <- getContext
      return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (max 1 (c^.availH)))

vPad :: Char -> Widget
vPad ch =
    Widget $ do
      c <- getContext
      return $ def & image .~ (V.charFill (c^.attr) ch (max 1 (c^.availW)) (c^.availH))

hFill :: Char -> Widget
hFill ch =
    Widget $ do
      c <- getContext
      return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (min (c^.availH) 1))

vFill :: Char -> Widget
vFill ch =
    Widget $ do
      c <- getContext
      return $ def & image .~ (V.charFill (c^.attr) ch (min (c^.availW) 1) (c^.availH))

hBox :: [(Widget, Priority)] -> Widget
hBox pairs =
    Widget $ do
      c <- getContext

      let pairsIndexed = zip [(0::Int)..] pairs
          his = filter (\p -> (snd $ snd p) == High) pairsIndexed
          lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

      renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> (render prim)) his

      let remainingWidth = c^.availW - (sum $ (^._2.image.(to V.imageWidth)) <$> renderedHis)
          widthPerLow = remainingWidth `div` length lows
          padFirst = if widthPerLow * length lows < remainingWidth
                     then remainingWidth - widthPerLow * length lows
                     else 0
          heightPerLow = maximum $ (^._2.image.(to V.imageHeight)) <$> renderedHis
          renderLow (i, (prim, _)) =
              let padding = (if i == 0 then padFirst else 0)
              in (i,) <$> (render $ vLimit heightPerLow $ hLimit (widthPerLow + padding) $ cropToContext prim)

      renderedLows <- mapM renderLow lows

      let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
          allResults = snd <$> rendered
          allImages = (^.image) <$> allResults
          allWidths = V.imageWidth <$> allImages
          allTranslatedVRs = for (zip [0..] allResults) $ \(i, result) ->
              let off = Location (offWidth, 0)
                  offWidth = sum $ take i allWidths
              in (addVisibilityOffset off result)^.visibilityRequests
          allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
              let off = Location (offWidth, 0)
                  offWidth = sum $ take i allWidths
              in (addCursorOffset off result)^.cursors

      cropResultToContext $ Result (V.horizCat allImages) (concat allTranslatedCursors) (concat allTranslatedVRs)

vBox :: [(Widget, Priority)] -> Widget
vBox pairs =
    Widget $ do
      c <- getContext

      let pairsIndexed = zip [(0::Int)..] pairs
          his = filter (\p -> (snd $ snd p) == High) pairsIndexed
          lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

      renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render prim) his

      let remainingHeight = c^.availH - (sum $ (^._2.image.(to V.imageHeight)) <$> renderedHis)
          heightPerLow = remainingHeight `div` length lows
          padFirst = if heightPerLow * length lows < remainingHeight
                     then remainingHeight - heightPerLow * length lows
                     else 0
          widthPerLow = maximum $ (^._2.image.(to V.imageWidth)) <$> renderedHis
          renderLow (i, (prim, _)) =
              let padding = if i == 0 then padFirst else 0
              in (i,) <$> (render $ vLimit (heightPerLow + padding) $ hLimit widthPerLow $ cropToContext prim)

      renderedLows <- mapM renderLow lows

      let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
          allResults = snd <$> rendered
          allImages = (^.image) <$> allResults
          allHeights = V.imageHeight <$> allImages
          allTranslatedVRs = for (zip [0..] allResults) $ \(i, result) ->
              let off = Location (0, offHeight)
                  offHeight = sum $ take i allHeights
              in (addVisibilityOffset off result)^.visibilityRequests
          allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
              let off = Location (0, offHeight)
                  offHeight = sum $ take i allHeights
              in (addCursorOffset off result)^.cursors

      cropResultToContext $ Result (V.vertCat allImages) (concat allTranslatedCursors) (concat allTranslatedVRs)

-- xxx crop cursors and VRs
hLimit :: Int -> Widget -> Widget
hLimit w p =
    Widget $ do
      withReaderT (& availW .~ w) $ render $ cropToContext p

-- xxx crop cursors and VRs
vLimit :: Int -> Widget -> Widget
vLimit h p =
    Widget $ do
      withReaderT (& availH .~ h) $ render $ cropToContext p

withAttrName :: AttrName -> Widget -> Widget
withAttrName an p =
    Widget $ do
      withReaderT (& ctxAttrName .~ an) (render p)

withAttrMappings :: [(AttrName, V.Attr)] -> Widget -> Widget
withAttrMappings ms p =
    Widget $ do
      withReaderT (& ctxAttrs %~ applyAttrMappings ms) (render p)

withDefaultAttr :: V.Attr -> Widget -> Widget
withDefaultAttr a p =
    Widget $ do
        withReaderT (& ctxAttrs %~ (setDefault a)) (render p)

forceAttr :: V.Attr -> Widget -> Widget
forceAttr a p =
    Widget $ do
        withReaderT (& ctxAttrs .~ (forceAttrMap a)) (render p)

raw :: V.Image -> Widget
raw img = Widget $ return $ def & image .~ img

translateBy :: Location -> Widget -> Widget
translateBy (Location (tw,th)) p =
    Widget $ do
      result <- render p
      return $ addCursorOffset (Location (tw, th)) $
               addVisibilityOffset (Location (tw, th)) $
               result & image %~ (V.translate tw th)

cropResultToContext :: Result -> RenderM Result
cropResultToContext result = do
    c <- getContext
    return $ result & image %~ (V.crop (c^.availW) (c^.availH))

cropToContext :: Widget -> Widget
cropToContext p =
    Widget $ (render p >>= cropResultToContext)

cropLeftBy :: Int -> Widget -> Widget
cropLeftBy cols p =
    Widget $ do
      result <- render p
      let amt = V.imageWidth (result^.image) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
      return $ addCursorOffset (Location (-1 * cols, 0)) $
               addVisibilityOffset (Location (-1 * cols, 0)) $
               result & image %~ cropped

cropRightBy :: Int -> Widget -> Widget
cropRightBy cols p =
    Widget $ do
      result <- render p
      let amt = V.imageWidth (result^.image) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
      -- xxx cursors / VRs
      return $ result & image %~ cropped

cropTopBy :: Int -> Widget -> Widget
cropTopBy rows p =
    Widget $ do
      result <- render p
      let amt = V.imageHeight (result^.image) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
      return $ addCursorOffset (Location (0, -1 * rows)) $
               addVisibilityOffset (Location (0, -1 * rows)) $
               result & image %~ cropped

cropBottomBy :: Int -> Widget -> Widget
cropBottomBy rows p =
    Widget $ do
      result <- render p
      let amt = V.imageHeight (result^.image) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
      -- xxx crop cursors / VRs
      return $ result & image %~ cropped

showCursor :: Name -> Location -> Widget -> Widget
showCursor n cloc p =
    Widget $ do
      result <- render p
      return $ result & cursors %~ (CursorLocation cloc (Just n):)

hRelease :: Widget -> Widget
hRelease p =
    Widget $ withReaderT (& availW .~ unrestricted) (render p) --- NB

vRelease :: Widget -> Widget
vRelease p =
    Widget $ withReaderT (& availH .~ unrestricted) (render p) --- NB

viewport :: Name -> ViewportType -> Widget -> Widget
viewport vpname typ p =
    Widget $ do
      -- First, update the viewport size.
      c <- getContext
      let newVp = VP 0 0 newSize
          newSize = (c^.availW, c^.availH)
          doInsert (Just vp) = Just $ vp & vpSize .~ newSize
          doInsert Nothing = Just newVp

      lift $ modify (& viewportMap %~ (M.alter doInsert vpname))

      -- Then render the sub-rendering with the rendering layout
      -- constraint released
      let release = case typ of
            Vertical -> vRelease
            Horizontal -> hRelease

      initialResult <- render $ release p

      -- If the sub-rendering requested visibility, update the scroll
      -- state accordingly
      when (not $ null $ initialResult^.visibilityRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMap.to (M.lookup vpname))
          -- XXX for now, just permit one request but we could permit
          -- many by computing the bounding rectangle over the submitted
          -- requests.
          let [rq] = initialResult^.visibilityRequests
              updatedVp = scrollToView typ rq vp
          lift $ modify (& viewportMap %~ (M.insert vpname updatedVp))

      -- If the rendering state includes any scrolling requests for this
      -- viewport, apply those
      reqs <- lift $ gets $ (^.scrollRequests)
      let relevantRequests = snd <$> filter (\(n, _) -> n == vpname) reqs
      when (not $ null relevantRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMap.to (M.lookup vpname))
          let [rq] = relevantRequests
              updatedVp = scrollTo typ rq (initialResult^.image) vp
          lift $ modify (& viewportMap %~ (M.insert vpname updatedVp))
          return ()

      -- Get the viewport state now that it has been updated.
      Just vp <- lift $ gets (M.lookup vpname . (^.viewportMap))

      -- Then perform a translation of the sub-rendering to fit into the
      -- viewport
      translated <- render $ translateBy (Location (-1 * vp^.vpLeft, -1 * vp^.vpTop)) $ Widget $ return initialResult

      -- Return the translated result with the visibility requests
      -- discarded
      render $ cropToContext $ ((Widget $ return $ translated & visibilityRequests .~ mempty) <+> hPad ' ') <=> vPad ' '

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
            Horizontal -> rq^.vrPosition.loc._1
            Vertical -> rq^.vrPosition.loc._2
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

visible :: Widget -> Widget
visible p =
    Widget $ do
      result <- render p
      let imageSize = ( result^.image.to V.imageWidth
                      , result^.image.to V.imageHeight
                      )
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if imageSize^._1 > 0 && imageSize^._2 > 0
               then result & visibilityRequests %~ (VR (Location (0, 0)) imageSize :)
               else result

visibleRegion :: Location -> V.DisplayRegion -> Widget -> Widget
visibleRegion vrloc sz p =
    Widget $ do
      result <- render p
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if sz^._1 > 0 && sz^._2 > 0
               then result & visibilityRequests %~ (VR vrloc sz :)
               else result

(<+>) :: Widget -> Widget -> Widget
(<+>) a b = hBox [(a, High), (b, High)]

(<<+) :: Widget -> Widget -> Widget
(<<+) a b = hBox [(a, High), (b, Low)]

(+>>) :: Widget -> Widget -> Widget
(+>>) a b = hBox [(a, Low), (b, High)]

(<=>) :: Widget -> Widget -> Widget
(<=>) a b = vBox [(a, High), (b, High)]

(<<=) :: Widget -> Widget -> Widget
(<<=) a b = vBox [(a, High), (b, Low)]

(=>>) :: Widget -> Widget -> Widget
(=>>) a b = vBox [(a, Low), (b, High)]
