{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Render.Internal
  ( Result(..)
  , image
  , cursors

  , Priority(..)
  , renderFinal
  , Render

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
  , saveSize
  , hRelease
  , vRelease
  , withLens
  , usingState
  , ensure
  )
where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (.=), (.~), (&), (%~), to, _2)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Data.Default
import qualified Data.Function as DF
import Data.List (sortBy)
import Control.Lens (Lens')
import Data.String (IsString(..))
import qualified Graphics.Vty as V

import Brick.Core (Location(..), CursorLocation(..), CursorName(..))
import Brick.Util (clOffset, for)

data Result =
    Result { _image :: V.Image
           , _cursors :: [CursorLocation]
           }
           deriving Show

data Context =
    Context { _attr :: V.Attr
            , _w :: Int
            , _h :: Int
            }

makeLenses ''Result
makeLenses ''Context

data Priority = High | Low
              deriving (Show, Eq)

type Render a = ReaderT Context (State a) Result

instance IsString (Render a) where
    fromString = txt

instance Default Result where
    def = Result V.emptyImage []

renderFinal :: [Render a]
            -> V.DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> a
            -> (a, V.Picture, Maybe CursorLocation)
renderFinal layerRenders sz chooseCursor st = (newState, pic, theCursor)
    where
        (layerResults, newState) = flip runState st $ sequence $ (\p -> runReaderT p ctx) <$> layerRenders
        ctx = Context V.defAttr (fst sz) (snd sz)
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.image) <$> layerResults
        layerCursors = (^.cursors) <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

addCursorOffset :: Location -> Result -> Result
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible (CursorLocation (Location (width, height)) _) = width >= 0 && height >= 0
    in r & cursors %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 1000

txt :: String -> Render a
txt s = do
    c <- ask
    return $ if c^.w > 0 && c^.h > 0
             then def & image .~ (V.crop (c^.w) (c^.h) $ V.string (c^.attr) s)
             else def

hPad :: Char -> Render a
hPad ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (c^.w) (max 1 (c^.h)))

vPad :: Char -> Render a
vPad ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (max 1 (c^.w)) (c^.h))

hFill :: Char -> Render a
hFill ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (c^.w) (min (c^.h) 1))

vFill :: Char -> Render a
vFill ch = do
    c <- ask
    return $ def & image .~ (V.charFill (c^.attr) ch (min (c^.w) 1) (c^.h))

hBox :: [(Render a, Priority)] -> Render a
hBox pairs = do
    c <- ask

    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim) his

    let remainingWidth = c^.w - (sum $ (^._2.image.(to V.imageWidth)) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (^._2.image.(to V.imageHeight)) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = (if i == 0 then padFirst else 0)
            in (i,) <$> (withReaderT (\v -> v & w .~ widthPerLow + padding
                                              & h .~ heightPerLow) prim)

    renderedLows <- mapM renderLow lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = (^.image) <$> allResults
        allWidths = V.imageWidth <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (offWidth, 0)
                offWidth = sum $ take i allWidths
            in (addCursorOffset off result)^.cursors

    return $ Result (V.horizCat allImages) (concat allTranslatedCursors)

vBox :: [(Render a, Priority)] -> Render a
vBox pairs = do
    c <- ask

    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim) his

    let remainingHeight = c^.h - (sum $ (^._2.image.(to V.imageHeight)) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (^._2.image.(to V.imageWidth)) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = if i == 0 then padFirst else 0
            in (i,) <$> (withReaderT (\v -> v & w .~ widthPerLow
                                              & h .~ (heightPerLow + padding)) prim)

    renderedLows <- mapM renderLow lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = (^.image) <$> allResults
        allHeights = V.imageHeight <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (0, offHeight)
                offHeight = sum $ take i allHeights
            in (addCursorOffset off result)^.cursors

    return $ Result (V.vertCat allImages) (concat allTranslatedCursors)

-- xxx crop cursors
hLimit :: Int -> Render a -> Render a
hLimit w' = withReaderT (& w .~ w')

-- xxx crop cursors
vLimit :: Int -> Render a -> Render a
vLimit h' = withReaderT (& h .~ h')

useAttr :: V.Attr -> Render a -> Render a
useAttr a = withReaderT (& attr .~ a)

raw :: V.Image -> Render a
raw img = do
    c <- ask
    return $ if c^.w > 0 && c^.h > 0
             then def & image .~ (V.crop (c^.w) (c^.h) img)
             else def

translate :: Location -> Render a -> Render a
translate (Location (tw,th)) p = do
    result <- p
    c <- ask
    return $ addCursorOffset (Location (tw, th)) $
             result & image %~ (V.crop (c^.w) (c^.h) . V.translate tw th)

cropLeftBy :: Int -> Render a -> Render a
cropLeftBy cols p = do
    result <- p
    let amt = V.imageWidth (result^.image) - cols
        cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
    return $ addCursorOffset (Location (-1 * cols, 0)) $ result & image %~ cropped

cropRightBy :: Int -> Render a -> Render a
cropRightBy cols p = do
    result <- p
    let amt = V.imageWidth (result^.image) - cols
        cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
    -- xxx cursors
    return $ result & image %~ cropped

cropTopBy :: Int -> Render a -> Render a
cropTopBy rows p = do
    result <- p
    let amt = V.imageHeight (result^.image) - rows
        cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
    return $ addCursorOffset (Location (0, -1 * rows)) $ result & image %~ cropped

cropBottomBy :: Int -> Render a -> Render a
cropBottomBy rows p = do
    result <- p
    let amt = V.imageHeight (result^.image) - rows
        cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
    -- xxx crop cursors
    return $ result & image %~ cropped

showCursor :: CursorName -> Location -> Render a -> Render a
showCursor n loc p = do
    result <- p
    return $ result & cursors %~ (CursorLocation loc (Just n):)

saveSize :: (V.DisplayRegion -> a -> a) -> Render a -> Render a
saveSize sizeSetter p = do
    result <- p
    let img = result^.image
        imgSz = (V.imageWidth img, V.imageHeight img)
    lift $ modify (sizeSetter imgSz)
    return result

hRelease :: Render a -> Render a
hRelease = withReaderT (& w .~ unrestricted) --- NB

vRelease :: Render a -> Render a
vRelease = withReaderT (& h .~ unrestricted) --- NB

withLens :: (Lens' a b) -> Render b -> Render a
withLens target p = do
    outerState <- lift get
    let oldInnerState = outerState^.target
    c <- ask
    let (result, newInnerState) = runState (runReaderT p c) oldInnerState
    target .= newInnerState
    return result

usingState :: (a -> Render a) -> Render a
usingState f = (lift get) >>= (\a -> f a)

ensure :: (a -> a) -> Render a -> Render a
ensure f p = do
    result <- p
    lift $ modify f
    return result
