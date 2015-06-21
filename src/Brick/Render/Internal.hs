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
  , Render
  , RenderM

  , Context
  , availW
  , availH
  , getActiveBorderStyle
  , getContext

  , withBorderStyle

  , ViewportType(..)

  , txt
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
type Render = RenderM Result

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

instance IsString Render where
    fromString = txt

instance Default Result where
    def = Result V.emptyImage [] []

getContext :: RenderM Context
getContext = ask

withBorderStyle :: BorderStyle -> Render -> Render
withBorderStyle bs = withReaderT (& activeBorderStyle .~ bs)

getActiveBorderStyle :: RenderM BorderStyle
getActiveBorderStyle = view activeBorderStyle

renderFinal :: AttrMap
            -> [Render]
            -> V.DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> RenderState
            -> (RenderState, V.Picture, Maybe CursorLocation)
renderFinal aMap layerRenders sz chooseCursor rs = (newRS, pic, theCursor)
    where
        (layerResults, newRS) = flip runState rs $ sequence $ (\p -> runReaderT p ctx) <$> (cropToContext <$> layerRenders)
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

txt :: String -> Render
txt s = do
    c <- getContext
    return $ def & image .~ (V.string (c^.attr) s)

hPad :: Char -> Render
hPad ch = do
    c <- getContext
    return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (max 1 (c^.availH)))

vPad :: Char -> Render
vPad ch = do
    c <- getContext
    return $ def & image .~ (V.charFill (c^.attr) ch (max 1 (c^.availW)) (c^.availH))

hFill :: Char -> Render
hFill ch = do
    c <- getContext
    return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (min (c^.availH) 1))

vFill :: Char -> Render
vFill ch = do
    c <- getContext
    return $ def & image .~ (V.charFill (c^.attr) ch (min (c^.availW) 1) (c^.availH))

hBox :: [(Render, Priority)] -> Render
hBox pairs = do
    c <- getContext

    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim) his

    let remainingWidth = c^.availW - (sum $ (^._2.image.(to V.imageWidth)) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (^._2.image.(to V.imageHeight)) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = (if i == 0 then padFirst else 0)
            in (i,) <$> (vLimit heightPerLow $ hLimit (widthPerLow + padding) $ cropToContext prim)

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

    cropToContext $ return $ Result (V.horizCat allImages) (concat allTranslatedCursors) (concat allTranslatedVRs)

vBox :: [(Render, Priority)] -> Render
vBox pairs = do
    c <- getContext

    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim) his

    let remainingHeight = c^.availH - (sum $ (^._2.image.(to V.imageHeight)) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (^._2.image.(to V.imageWidth)) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = if i == 0 then padFirst else 0
            in (i,) <$> (vLimit (heightPerLow + padding) $ hLimit widthPerLow $ cropToContext prim)

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

    cropToContext $ return $ Result (V.vertCat allImages) (concat allTranslatedCursors) (concat allTranslatedVRs)

-- xxx crop cursors and VRs
hLimit :: Int -> Render -> Render
hLimit w p = withReaderT (& availW .~ w) $ cropToContext p

-- xxx crop cursors and VRs
vLimit :: Int -> Render -> Render
vLimit h p = withReaderT (& availH .~ h) $ cropToContext p

withAttrName :: AttrName -> Render -> Render
withAttrName an = withReaderT (& ctxAttrName .~ an)

withAttrMappings :: [(AttrName, V.Attr)] -> Render -> Render
withAttrMappings ms =
    withReaderT (& ctxAttrs %~ applyAttrMappings ms)

withDefaultAttr :: V.Attr -> Render -> Render
withDefaultAttr a = withReaderT (& ctxAttrs %~ (setDefault a))

forceAttr :: V.Attr -> Render -> Render
forceAttr a = withReaderT (& ctxAttrs .~ (forceAttrMap a))

raw :: V.Image -> Render
raw img = return $ def & image .~ img

translateBy :: Location -> Render -> Render
translateBy (Location (tw,th)) p = do
    result <- p
    return $ addCursorOffset (Location (tw, th)) $
             addVisibilityOffset (Location (tw, th)) $
             result & image %~ (V.translate tw th)

cropToContext :: Render -> Render
cropToContext p = do
    result <- p
    c <- getContext
    return $ result & image %~ (V.crop (c^.availW) (c^.availH))

cropLeftBy :: Int -> Render -> Render
cropLeftBy cols p = do
    result <- p
    let amt = V.imageWidth (result^.image) - cols
        cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
    return $ addCursorOffset (Location (-1 * cols, 0)) $
             addVisibilityOffset (Location (-1 * cols, 0)) $
             result & image %~ cropped

cropRightBy :: Int -> Render -> Render
cropRightBy cols p = do
    result <- p
    let amt = V.imageWidth (result^.image) - cols
        cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
    -- xxx cursors / VRs
    return $ result & image %~ cropped

cropTopBy :: Int -> Render -> Render
cropTopBy rows p = do
    result <- p
    let amt = V.imageHeight (result^.image) - rows
        cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
    return $ addCursorOffset (Location (0, -1 * rows)) $
             addVisibilityOffset (Location (0, -1 * rows)) $
             result & image %~ cropped

cropBottomBy :: Int -> Render -> Render
cropBottomBy rows p = do
    result <- p
    let amt = V.imageHeight (result^.image) - rows
        cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
    -- xxx crop cursors / VRs
    return $ result & image %~ cropped

showCursor :: Name -> Location -> Render -> Render
showCursor n cloc p = do
    result <- p
    return $ result & cursors %~ (CursorLocation cloc (Just n):)

hRelease :: Render -> Render
hRelease = withReaderT (& availW .~ unrestricted) --- NB

vRelease :: Render -> Render
vRelease = withReaderT (& availH .~ unrestricted) --- NB

viewport :: Name -> ViewportType -> Render -> Render
viewport vpname typ p = do
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

    initialResult <- release p

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
    translated <- translateBy (Location (-1 * vp^.vpLeft, -1 * vp^.vpTop)) $ return initialResult

    -- Return the translated result with the visibility requests
    -- discarded
    cropToContext $ ((return $ translated & visibilityRequests .~ mempty) <+> hPad ' ') <=> vPad ' '

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

visible :: Render -> Render
visible p = do
    result <- p
    let imageSize = ( result^.image.to V.imageWidth
                    , result^.image.to V.imageHeight
                    )
    -- The size of the image to be made visible in a viewport must have
    -- non-zero size in both dimensions.
    return $ if imageSize^._1 > 0 && imageSize^._2 > 0
             then result & visibilityRequests %~ (VR (Location (0, 0)) imageSize :)
             else result

visibleRegion :: Location -> V.DisplayRegion -> Render -> Render
visibleRegion vrloc sz p = do
    result <- p
    -- The size of the image to be made visible in a viewport must have
    -- non-zero size in both dimensions.
    return $ if sz^._1 > 0 && sz^._2 > 0
             then result & visibilityRequests %~ (VR vrloc sz :)
             else result

(<+>) :: Render -> Render -> Render
(<+>) a b = hBox [(a, High), (b, High)]

(<<+) :: Render -> Render -> Render
(<<+) a b = hBox [(a, High), (b, Low)]

(+>>) :: Render -> Render -> Render
(+>>) a b = hBox [(a, Low), (b, High)]

(<=>) :: Render -> Render -> Render
(<=>) a b = vBox [(a, High), (b, High)]

(<<=) :: Render -> Render -> Render
(<<=) a b = vBox [(a, High), (b, Low)]

(=>>) :: Render -> Render -> Render
(=>>) a b = vBox [(a, Low), (b, High)]
