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

type Prim a = Primitive a

data Primitive a = Txt !String
                 | HPad !Char
                 | VPad !Char
                 | HFill !Char
                 | VFill !Char
                 | HBox ![(Prim a, Priority)]
                 | VBox ![(Prim a, Priority)]
                 | HLimit !Int !(Prim a)
                 | VLimit !Int !(Prim a)
                 | UseAttr !V.Attr !(Prim a)
                 | Raw !V.Image
                 | Translate !Location !(Prim a)
                 | CropLeftBy !Int !(Prim a)
                 | CropRightBy !Int !(Prim a)
                 | CropTopBy !Int !(Prim a)
                 | CropBottomBy !Int !(Prim a)
                 | ShowCursor !CursorName !Location !(Prim a)
                 | SaveSize (V.DisplayRegion -> a -> a) !(Prim a)
                 | HRelease !(Prim a)
                 | VRelease !(Prim a)
                 | forall b. With (Lens' a b) (Prim b)
                 | ReadState (a -> Prim a)

instance IsString (Prim a) where
    fromString = Txt

instance Default Render where
    def = Render V.emptyImage []

renderFinal :: [Prim a]
            -> V.DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> a
            -> (a, V.Picture, Maybe CursorLocation)
renderFinal layerPrims sz chooseCursor st = (newState, pic, theCursor)
    where
        (layerResults, newState) = flip runState st $ sequence $ render ctx <$> layerPrims
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

render :: Context -> Prim a -> State a Render
render = renderPrim

renderPrim :: Context -> Primitive a -> State a Render
renderPrim c (ReadState f) = get >>= (render c . f)
renderPrim c (With target p) = do
    outerState <- get
    let oldInnerState = outerState^.target
        (result, newInnerState) = runState (render c p) oldInnerState
    target .= newInnerState
    return result
renderPrim c (Txt s) =
    return $ if c^.w > 0 && c^.h > 0
             then setImage (V.crop (c^.w) (c^.h) $ V.string (c^.attr) s) def
             else def
renderPrim c (Raw img) =
    return $ if c^.w > 0 && c^.h > 0
             then setImage (V.crop (c^.w) (c^.h) img) def
             else def
renderPrim c (CropLeftBy cols p) = do
    result <- render c p
    let img = image result
        amt = V.imageWidth img - cols
        cropped = if amt < 0 then V.emptyImage else V.cropLeft amt img
    return $ addCursorOffset (Location (-1 * cols, 0)) $
             setImage cropped result
renderPrim c (CropRightBy cols p) = do
    result <- render c p
    let img = image result
        amt = V.imageWidth img - cols
        cropped = if amt < 0 then V.emptyImage else V.cropRight amt img
    -- xxx cursors
    return $ setImage cropped result
renderPrim c (CropTopBy rows p) = do
    result <- render c p
    let img = image result
        amt = V.imageHeight img - rows
        cropped = if amt < 0 then V.emptyImage else V.cropTop amt img
    return $ addCursorOffset (Location (0, -1 * rows)) $
             setImage cropped result
renderPrim c (CropBottomBy rows p) = do
    result <- render c p
    let img = image result
        amt = V.imageHeight img - rows
        cropped = if amt < 0 then V.emptyImage else V.cropBottom amt img
    -- xxx crop cursors
    return $ setImage cropped result
renderPrim c (HPad ch) = return $ setImage (V.charFill (c^.attr) ch (c^.w) (max 1 (c^.h))) def
renderPrim c (VPad ch) = return $ setImage (V.charFill (c^.attr) ch (max 1 (c^.w)) (c^.h)) def
renderPrim c (HFill ch) = return $ setImage (V.charFill (c^.attr) ch (c^.w) (min (c^.h) 1)) def
renderPrim c (VFill ch) = return $ setImage (V.charFill (c^.attr) ch (min (c^.w) 1) (c^.h)) def
renderPrim c (HLimit w' p) =
    -- xxx crop cursors
    render (c & w .~ w') p
renderPrim c (VLimit h' p) =
    -- xxx crop cursors
    render (c & h .~ h') p
renderPrim c (HRelease p) = render (c & w .~ unrestricted) p --- NB
renderPrim c (VRelease p) = render (c & h .~ unrestricted) p --- NB
renderPrim c (UseAttr a p) = render (c & attr .~ a) p
renderPrim c (Translate (Location (tw,th)) p) = do
    result <- render c p
    let img = image result
    return $ addCursorOffset (Location (tw, th)) $
             setImage (V.crop (c^.w) (c^.h) $ V.translate tw th img) result
renderPrim c (ShowCursor n loc p) = do
    result <- render c p
    return $ result { cursors = (CursorLocation loc (Just n)):cursors result }
renderPrim c (SaveSize sizeSetter p) = do
    result <- render c p
    let img = image result
        imgSz = (V.imageWidth img, V.imageHeight img)
    modify (sizeSetter imgSz)
    return result
renderPrim c (HBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render c prim) his

    let remainingWidth = c^.w - (sum $ (V.imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (V.imageHeight . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = (if i == 0 then padFirst else 0)
            in (i,) <$> render (c & w .~ widthPerLow + padding
                                  & h .~ heightPerLow)
                               prim

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

renderPrim c (VBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render c prim) his

    let remainingHeight = c^.h - (sum $ (V.imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (V.imageWidth . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = if i == 0 then padFirst else 0
            in (i,) <$> render (c & w .~ widthPerLow
                                  & h .~ (heightPerLow + padding))
                               prim

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

txt :: String -> Prim a
txt = Txt

hPad :: Char -> Prim a
hPad = HPad

vPad :: Char -> Prim a
vPad = VPad

hFill :: Char -> Prim a
hFill = HFill

vFill :: Char -> Prim a
vFill = VFill

hBox :: [(Prim a, Priority)] -> Prim a
hBox = HBox

vBox :: [(Prim a, Priority)] -> Prim a
vBox = VBox

hLimit :: Int -> Prim a -> Prim a
hLimit l p = HLimit l p

vLimit :: Int -> Prim a -> Prim a
vLimit l p = VLimit l p

useAttr :: V.Attr -> Prim a -> Prim a
useAttr a p = UseAttr a p

raw :: V.Image -> Prim a
raw = Raw

translate :: Location -> Prim a -> Prim a
translate l p = Translate l p

cropLeftBy :: Int -> Prim a -> Prim a
cropLeftBy a p = CropLeftBy a p

cropRightBy :: Int -> Prim a -> Prim a
cropRightBy a p = CropRightBy a p

cropTopBy :: Int -> Prim a -> Prim a
cropTopBy a p = CropTopBy a p

cropBottomBy :: Int -> Prim a -> Prim a
cropBottomBy a p = CropBottomBy a p

showCursor :: CursorName -> Location -> Prim a -> Prim a
showCursor n l p = ShowCursor n l p

saveSize :: (V.DisplayRegion -> a -> a) -> Prim a -> Prim a
saveSize f p = SaveSize f p

hRelease :: Prim a -> Prim a
hRelease = HRelease

vRelease :: Prim a -> Prim a
vRelease = VRelease

with :: (Lens' a b) -> Prim b -> Prim a
with l p = With l p

readState :: (a -> Prim a) -> Prim a
readState = ReadState
