{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Prim.Internal
  ( Render(..)
  , Priority(..)
  , renderFinal
  , Prim

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
  , readState
  , apply
  )
where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (.=), (.~), (&))
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

data Render =
    Render { image :: V.Image
           , cursors :: [CursorLocation]
           }
           deriving Show

data Context =
    Context { _attr :: V.Attr
            , _w :: Int
            , _h :: Int
            }

makeLenses ''Context

data Priority = High | Low
              deriving (Show, Eq)

type Prim a = ReaderT Context (State a) Render

instance IsString (Prim a) where
    fromString = txt

instance Default Render where
    def = Render V.emptyImage []

renderFinal :: [Prim a]
            -> V.DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> a
            -> (a, V.Picture, Maybe CursorLocation)
renderFinal layerPrims sz chooseCursor st = (newState, pic, theCursor)
    where
        (layerResults, newState) = flip runState st $ sequence $ (\p -> runReaderT p ctx) <$> layerPrims
        ctx = Context V.defAttr (fst sz) (snd sz)
        pic = V.picForLayers $ uncurry V.resize sz <$> image <$> layerResults
        layerCursors = cursors <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

addCursorOffset :: Location -> Render -> Render
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible (CursorLocation (Location (width, height)) _) = width >= 0 && height >= 0
    in r { cursors = onlyVisible $ (`clOffset` off) <$> cursors r
         }

setImage :: V.Image -> Render -> Render
setImage i r = r { image = i }

unrestricted :: Int
unrestricted = 1000

txt :: String -> Prim a
txt s = do
    c <- ask
    return $ if c^.w > 0 && c^.h > 0
             then setImage (V.crop (c^.w) (c^.h) $ V.string (c^.attr) s) def
             else def

hPad :: Char -> Prim a
hPad ch = do
    c <- ask
    return $ setImage (V.charFill (c^.attr) ch (c^.w) (max 1 (c^.h))) def

vPad :: Char -> Prim a
vPad ch = do
    c <- ask
    return $ setImage (V.charFill (c^.attr) ch (max 1 (c^.w)) (c^.h)) def

hFill :: Char -> Prim a
hFill ch = do
    c <- ask
    return $ setImage (V.charFill (c^.attr) ch (c^.w) (min (c^.h) 1)) def

vFill :: Char -> Prim a
vFill ch = do
    c <- ask
    return $ setImage (V.charFill (c^.attr) ch (min (c^.w) 1) (c^.h)) def

hBox :: [(Prim a, Priority)] -> Prim a
hBox pairs = do
    c <- ask

    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim) his

    let remainingWidth = c^.w - (sum $ (V.imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (V.imageHeight . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = (if i == 0 then padFirst else 0)
            in (i,) <$> (withReaderT (\v -> v & w .~ widthPerLow + padding
                                              & h .~ heightPerLow) prim)

    renderedLows <- mapM renderLow lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allWidths = V.imageWidth <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (offWidth, 0)
                offWidth = sum $ take i allWidths
            in cursors $ addCursorOffset off result

    return $ Render (V.horizCat allImages) (concat allTranslatedCursors)

vBox :: [(Prim a, Priority)] -> Prim a
vBox pairs = do
    c <- ask

    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim) his

    let remainingHeight = c^.h - (sum $ (V.imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (V.imageWidth . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = if i == 0 then padFirst else 0
            in (i,) <$> (withReaderT (\v -> v & w .~ widthPerLow
                                              & h .~ (heightPerLow + padding)) prim)

    renderedLows <- mapM renderLow lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allHeights = V.imageHeight <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (0, offHeight)
                offHeight = sum $ take i allHeights
            in cursors $ addCursorOffset off result

    return $ Render (V.vertCat allImages) (concat allTranslatedCursors)

-- xxx crop cursors
hLimit :: Int -> Prim a -> Prim a
hLimit w' = withReaderT (& w .~ w')

-- xxx crop cursors
vLimit :: Int -> Prim a -> Prim a
vLimit h' = withReaderT (& h .~ h')

useAttr :: V.Attr -> Prim a -> Prim a
useAttr a = withReaderT (& attr .~ a)

raw :: V.Image -> Prim a
raw img = do
    c <- ask
    return $ if c^.w > 0 && c^.h > 0
             then setImage (V.crop (c^.w) (c^.h) img) def
             else def

translate :: Location -> Prim a -> Prim a
translate (Location (tw,th)) p = do
    result <- p
    c <- ask
    let img = image result
    return $ addCursorOffset (Location (tw, th)) $
             setImage (V.crop (c^.w) (c^.h) $ V.translate tw th img) result

cropLeftBy :: Int -> Prim a -> Prim a
cropLeftBy cols p = do
    result <- p
    let img = image result
        amt = V.imageWidth img - cols
        cropped = if amt < 0 then V.emptyImage else V.cropLeft amt img
    return $ addCursorOffset (Location (-1 * cols, 0)) $
             setImage cropped result

cropRightBy :: Int -> Prim a -> Prim a
cropRightBy cols p = do
    result <- p
    let img = image result
        amt = V.imageWidth img - cols
        cropped = if amt < 0 then V.emptyImage else V.cropRight amt img
    -- xxx cursors
    return $ setImage cropped result

cropTopBy :: Int -> Prim a -> Prim a
cropTopBy rows p = do
    result <- p
    let img = image result
        amt = V.imageHeight img - rows
        cropped = if amt < 0 then V.emptyImage else V.cropTop amt img
    return $ addCursorOffset (Location (0, -1 * rows)) $
             setImage cropped result

cropBottomBy :: Int -> Prim a -> Prim a
cropBottomBy rows p = do
    result <- p
    let img = image result
        amt = V.imageHeight img - rows
        cropped = if amt < 0 then V.emptyImage else V.cropBottom amt img
    -- xxx crop cursors
    return $ setImage cropped result

showCursor :: CursorName -> Location -> Prim a -> Prim a
showCursor n loc p = do
    result <- p
    return $ result { cursors = (CursorLocation loc (Just n)):cursors result }

saveSize :: (V.DisplayRegion -> a -> a) -> Prim a -> Prim a
saveSize sizeSetter p = do
    result <- p
    let img = image result
        imgSz = (V.imageWidth img, V.imageHeight img)
    lift $ modify (sizeSetter imgSz)
    return result

hRelease :: Prim a -> Prim a
hRelease = withReaderT (& w .~ unrestricted) --- NB

vRelease :: Prim a -> Prim a
vRelease = withReaderT (& h .~ unrestricted) --- NB

withLens :: (Lens' a b) -> Prim b -> Prim a
withLens target p = do
    outerState <- lift get
    let oldInnerState = outerState^.target
    c <- ask
    let (result, newInnerState) = runState (runReaderT p c) oldInnerState
    target .= newInnerState
    return result

readState :: (a -> Prim a) -> Prim a
readState f = (lift get) >>= (\a -> f a)

apply :: Prim a -> (a -> a) -> Prim a
apply p f = do
    result <- p
    lift $ modify f
    return result
