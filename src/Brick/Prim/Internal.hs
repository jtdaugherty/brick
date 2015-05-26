{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , with
  , readState
  )
where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (.=), (.~), (&))
import Control.Monad.Trans.State.Lazy
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

type Prim a = Context -> State a Render

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
        (layerResults, newState) = flip runState st $ sequence $ layerPrims <*> (pure ctx)
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
txt s c =
    return $ if c^.w > 0 && c^.h > 0
             then setImage (V.crop (c^.w) (c^.h) $ V.string (c^.attr) s) def
             else def

hPad :: Char -> Prim a
hPad ch c = return $ setImage (V.charFill (c^.attr) ch (c^.w) (max 1 (c^.h))) def

vPad :: Char -> Prim a
vPad ch c = return $ setImage (V.charFill (c^.attr) ch (max 1 (c^.w)) (c^.h)) def

hFill :: Char -> Prim a
hFill ch c = return $ setImage (V.charFill (c^.attr) ch (c^.w) (min (c^.h) 1)) def

vFill :: Char -> Prim a
vFill ch c = return $ setImage (V.charFill (c^.attr) ch (min (c^.w) 1) (c^.h)) def

hBox :: [(Prim a, Priority)] -> Prim a
hBox pairs c = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim c) his

    let remainingWidth = c^.w - (sum $ (V.imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (V.imageHeight . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = (if i == 0 then padFirst else 0)
            in (i,) <$> prim (c & w .~ widthPerLow + padding
                                & h .~ heightPerLow)

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
vBox pairs c = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> prim c) his

    let remainingHeight = c^.h - (sum $ (V.imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (V.imageWidth . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = if i == 0 then padFirst else 0
            in (i,) <$> prim (c & w .~ widthPerLow
                                & h .~ (heightPerLow + padding))

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

hLimit :: Int -> Prim a -> Prim a
hLimit w' p c =
    -- xxx crop cursors
    p (c & w .~ w')

vLimit :: Int -> Prim a -> Prim a
vLimit h' p c =
    -- xxx crop cursors
    p (c & h .~ h')

useAttr :: V.Attr -> Prim a -> Prim a
useAttr a p c = p (c & attr .~ a)

raw :: V.Image -> Prim a
raw img c =
    return $ if c^.w > 0 && c^.h > 0
             then setImage (V.crop (c^.w) (c^.h) img) def
             else def

translate :: Location -> Prim a -> Prim a
translate (Location (tw,th)) p c = do
    result <- p c
    let img = image result
    return $ addCursorOffset (Location (tw, th)) $
             setImage (V.crop (c^.w) (c^.h) $ V.translate tw th img) result

cropLeftBy :: Int -> Prim a -> Prim a
cropLeftBy cols p c = do
    result <- p c
    let img = image result
        amt = V.imageWidth img - cols
        cropped = if amt < 0 then V.emptyImage else V.cropLeft amt img
    return $ addCursorOffset (Location (-1 * cols, 0)) $
             setImage cropped result

cropRightBy :: Int -> Prim a -> Prim a
cropRightBy cols p c = do
    result <- p c
    let img = image result
        amt = V.imageWidth img - cols
        cropped = if amt < 0 then V.emptyImage else V.cropRight amt img
    -- xxx cursors
    return $ setImage cropped result

cropTopBy :: Int -> Prim a -> Prim a
cropTopBy rows p c = do
    result <- p c
    let img = image result
        amt = V.imageHeight img - rows
        cropped = if amt < 0 then V.emptyImage else V.cropTop amt img
    return $ addCursorOffset (Location (0, -1 * rows)) $
             setImage cropped result

cropBottomBy :: Int -> Prim a -> Prim a
cropBottomBy rows p c = do
    result <- p c
    let img = image result
        amt = V.imageHeight img - rows
        cropped = if amt < 0 then V.emptyImage else V.cropBottom amt img
    -- xxx crop cursors
    return $ setImage cropped result

showCursor :: CursorName -> Location -> Prim a -> Prim a
showCursor n loc p c = do
    result <- p c
    return $ result { cursors = (CursorLocation loc (Just n)):cursors result }

saveSize :: (V.DisplayRegion -> a -> a) -> Prim a -> Prim a
saveSize sizeSetter p c = do
    result <- p c
    let img = image result
        imgSz = (V.imageWidth img, V.imageHeight img)
    modify (sizeSetter imgSz)
    return result

hRelease :: Prim a -> Prim a
hRelease p c = p (c & w .~ unrestricted) --- NB

vRelease :: Prim a -> Prim a
vRelease p c = p (c & h .~ unrestricted) --- NB

with :: (Lens' a b) -> Prim b -> Prim a
with target p c = do
    outerState <- get
    let oldInnerState = outerState^.target
        (result, newInnerState) = runState (p c) oldInnerState
    target .= newInnerState
    return result

readState :: (a -> Prim a) -> Prim a
readState f c = get >>= (\a -> f a c)
