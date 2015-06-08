{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Render.Internal
  ( Result(..)
  , image
  , cursors

  , RenderState(..)

  , Priority(..)
  , renderFinal
  , Render

  , Context
  , availW
  , availH

  , ViewportType(..)

  , txt
  , hPad
  , vPad
  , hFill
  , vFill
  , hBox
  , vBox
  , hLimit
  , vLimit
  , useAttr
  , raw
  , translate
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy
  , showCursor
  , viewport
  , visible
  )
where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (.~), (&), (%~), to, _1, _2)
import Control.Monad (when)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Data.Default
import Data.Monoid ((<>), mempty)
import qualified Data.Map as M
import qualified Data.Function as DF
import Data.List (sortBy)
import Control.Lens (Lens')
import Data.String (IsString(..))
import qualified Graphics.Vty as V

import Brick.Core (Location(..), loc, CursorLocation(..), CursorName(..))
import Brick.Util (clOffset, for)

import qualified Debug.Trace as D

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
    Context { _attr :: V.Attr
            , _availW :: Int
            , _availH :: Int
            }

data Priority = High | Low
              deriving (Show, Eq)

type Render = ReaderT Context (State RenderState) Result

data RenderState =
    RS { _viewportMap :: M.Map String Viewport
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

renderFinal :: [Render]
            -> V.DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> RenderState
            -> (RenderState, V.Picture, Maybe CursorLocation)
renderFinal layerRenders sz chooseCursor rs = (newRS, pic, theCursor)
    where
        (layerResults, newRS) = flip runState rs $ sequence $ (\p -> runReaderT p ctx) <$> (cropToContext <$> layerRenders)
        ctx = Context V.defAttr (fst sz) (snd sz)
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.image) <$> layerResults
        layerCursors = (^.cursors) <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

addVisibilityOffset :: Location -> Result -> Result
addVisibilityOffset off r =
    let addOffset vrs = (& vrPosition %~ (off <>)) <$> vrs
    in r & visibilityRequests %~ addOffset

addCursorOffset :: Location -> Result -> Result
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible (CursorLocation (Location (width, height)) _) = width >= 0 && height >= 0
    in r & cursors %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 1000

txt :: String -> Render
txt s = do
    c <- ask
    return $ def & image .~ (V.string (c^.attr) s)

hPad :: Char -> Render
hPad ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (max 1 (c^.availH)))

vPad :: Char -> Render
vPad ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (max 1 (c^.availW)) (c^.availH))

hFill :: Char -> Render
hFill ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (c^.availW) (min (c^.availH) 1))

vFill :: Char -> Render
vFill ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (min (c^.availW) 1) (c^.availH))

hBox :: [(Render, Priority)] -> Render
hBox pairs = do
    c <- ask

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
    c <- ask

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

useAttr :: V.Attr -> Render -> Render
useAttr a = withReaderT (& attr .~ a)

raw :: V.Image -> Render
raw img = do
    c <- ask
    return $ if c^.availW > 0 && c^.availH > 0
             then def & image .~ img
             else def

-- xxx find another name that doesn't clash with Vty's translate function
translate :: Location -> Render -> Render
translate (Location (tw,th)) p = do
    result <- p
    return $ addCursorOffset (Location (tw, th)) $
             addVisibilityOffset (Location (tw, th)) $
             result & image %~ (V.translate tw th)

cropToContext :: Render -> Render
cropToContext p = do
    result <- p
    c <- ask
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

showCursor :: CursorName -> Location -> Render -> Render
showCursor n cloc p = do
    result <- p
    return $ result & cursors %~ (CursorLocation cloc (Just n):)

hRelease :: Render -> Render
hRelease = withReaderT (& availW .~ unrestricted) --- NB

vRelease :: Render -> Render
vRelease = withReaderT (& availH .~ unrestricted) --- NB

viewport :: String -> ViewportType -> Render -> Render
viewport vpname typ p = do
    -- First, update the viewport size.
    c <- ask
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
        lift $ D.trace (show (vpname, rq, newVp)) $ modify (& viewportMap %~ (M.insert vpname updatedVp))

    -- Get the viewport state now that it has been updated.
    Just vp <- lift $ gets (M.lookup vpname . (^.viewportMap))

    -- Then perform a translation of the sub-rendering to fit into the
    -- viewport
    translated <- translate (Location (-1 * vp^.vpLeft, -1 * vp^.vpTop)) $ return initialResult

    -- Return the translated result with the visibility requests
    -- discarded
    cropToContext $ return $ translated & visibilityRequests .~ mempty

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
