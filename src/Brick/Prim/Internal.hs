{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Prim.Internal
  ( Render(..)
  , renderFinal
  )
where

import Control.Applicative
import Control.Lens (makeLenses, (^.), (.=), (.~), (&))
import Control.Monad.Trans.State.Lazy
import Data.Default
import qualified Data.Function as DF
import Data.List (sortBy)
import Graphics.Vty

import Brick.Core (Location(..), CursorLocation(..))
import Brick.Prim
import Brick.Util (clOffset, for)

data Render =
    Render { image :: !Image
           , cursors :: ![CursorLocation]
           }
           deriving Show

data Context =
    Context { _attr :: Attr
            , _w :: Int
            , _h :: Int
            }

makeLenses ''Context

instance Default Render where
    def = Render emptyImage []

renderFinal :: [Prim a]
            -> DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> a
            -> (a, Picture, Maybe CursorLocation)
renderFinal layerPrims sz chooseCursor st = (newState, pic, theCursor)
    where
        (layerResults, newState) = flip runState st $ sequence $ render ctx <$> layerPrims
        ctx = Context defAttr (fst sz) (snd sz)
        pic = picForLayers $ uncurry resize sz <$> image <$> layerResults
        layerCursors = cursors <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

addCursorOffset :: Location -> Render -> Render
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible (CursorLocation (Location (width, height)) _) = width >= 0 && height >= 0
    in r { cursors = onlyVisible $ (`clOffset` off) <$> cursors r
         }

setImage :: Image -> Render -> Render
setImage i r = r { image = i }

unrestricted :: Int
unrestricted = 1000

render :: Context -> Prim a -> State a Render
render c (With target f) = do
    outerState <- get
    let innerPrim = f oldInnerState
        oldInnerState = outerState^.target
        (innerRender, newInnerState) = runState (render c innerPrim) oldInnerState
    target .= newInnerState
    return innerRender
render c (Txt s) =
    return $ if c^.w > 0 && c^.h > 0
             then setImage (crop (c^.w) (c^.h) $ string (c^.attr) s) def
             else def
render c (Raw img) =
    return $ if c^.w > 0 && c^.h > 0
             then setImage (crop (c^.w) (c^.h) img) def
             else def
render c (CropLeftBy cols p) = do
    result <- render c p
    let img = image result
        amt = imageWidth img - cols
        cropped = if amt < 0 then emptyImage else cropLeft amt img
    return $ addCursorOffset (Location (-1 * cols, 0)) $
             setImage cropped result
render c (CropRightBy cols p) = do
    result <- render c p
    let img = image result
        amt = imageWidth img - cols
        cropped = if amt < 0 then emptyImage else cropRight amt img
    -- xxx cursors
    return $ setImage cropped result
render c (CropTopBy rows p) = do
    result <- render c p
    let img = image result
        amt = imageHeight img - rows
        cropped = if amt < 0 then emptyImage else cropTop amt img
    return $ addCursorOffset (Location (0, -1 * rows)) $
             setImage cropped result
render c (CropBottomBy rows p) = do
    result <- render c p
    let img = image result
        amt = imageHeight img - rows
        cropped = if amt < 0 then emptyImage else cropBottom amt img
    -- xxx crop cursors
    return $ setImage cropped result
render c (HPad ch) = return $ setImage (charFill (c^.attr) ch (c^.w) (max 1 (c^.h))) def
render c (VPad ch) = return $ setImage (charFill (c^.attr) ch (max 1 (c^.w)) (c^.h)) def
render c (HFill ch) = return $ setImage (charFill (c^.attr) ch (c^.w) (min (c^.h) 1)) def
render c (VFill ch) = return $ setImage (charFill (c^.attr) ch (min (c^.w) 1) (c^.h)) def
render c (HLimit w' p) =
    -- xxx crop cursors
    render (c & w .~ w') p
render c (VLimit h' p) =
    -- xxx crop cursors
    render (c & h .~ h') p
render c (HRelease p) = render (c & w .~ unrestricted) p --- NB
render c (VRelease p) = render (c & h .~ unrestricted) p --- NB
render c (UseAttr a p) = render (c & attr .~ a) p
render c (Translate (Location (tw,th)) p) = do
    result <- render c p
    let img = image result
    return $ addCursorOffset (Location (tw, th)) $
             setImage (crop (c^.w) (c^.h) $ translate tw th img) result
render c (ShowCursor n loc p) = do
    result <- render c p
    return $ result { cursors = (CursorLocation loc (Just n)):cursors result }
render c (SetSize sizeSetter p) = do
    result <- render c p
    let img = image result
        imgSz = (imageWidth img, imageHeight img)
    modify (sizeSetter imgSz)
    return result
render c (HBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render c prim) his

    let remainingWidth = c^.w - (sum $ (imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (imageHeight . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = (if i == 0 then padFirst else 0)
            in (i,) <$> render (c & w .~ widthPerLow + padding
                                  & h .~ heightPerLow)
                               prim

    renderedLows <- mapM renderLow lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allWidths = imageWidth <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (offWidth, 0)
                offWidth = sum $ take i allWidths
            in cursors $ addCursorOffset off result

    return $ Render (horizCat allImages) (concat allTranslatedCursors)

render c (VBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render c prim) his

    let remainingHeight = c^.h - (sum $ (imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (imageWidth . image . snd) <$> renderedHis
        renderLow (i, (prim, _)) =
            let padding = if i == 0 then padFirst else 0
            in (i,) <$> render (c & w .~ widthPerLow
                                  & h .~ (heightPerLow + padding))
                               prim

    renderedLows <- mapM renderLow lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allHeights = imageHeight <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (0, offHeight)
                offHeight = sum $ take i allHeights
            in cursors $ addCursorOffset off result

    return $ Render (vertCat allImages) (concat allTranslatedCursors)
