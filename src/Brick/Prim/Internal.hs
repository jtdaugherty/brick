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
        (layerResults, newState) = flip runState st $ sequence $ render sz ctx <$> layerPrims
        ctx = Context defAttr
        pic = picForLayers $ uncurry resize sz <$> image <$> layerResults
        layerCursors = cursors <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

addCursorOffset :: Location -> Render -> Render
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible (CursorLocation (Location (w, h)) _) = w >= 0 && h >= 0
    in r { cursors = onlyVisible $ (`clOffset` off) <$> cursors r
         }

setImage :: Image -> Render -> Render
setImage i r = r { image = i }

unrestricted :: Int
unrestricted = 1000

render :: DisplayRegion -> Context -> Prim a -> State a Render
render sz c (With target f) = do
    outerState <- get
    let innerPrim = f oldInnerState
        oldInnerState = outerState^.target
        (innerRender, newInnerState) = runState (render sz c innerPrim) oldInnerState
    target .= newInnerState
    return innerRender
render (w, h) c (Txt s) =
    return $ if w > 0 && h > 0
             then setImage (crop w h $ string (c^.attr) s) def
             else def
render (w, h) _ (Raw img) =
    return $ if w > 0 && h > 0
             then setImage (crop w h img) def
             else def
render (w, h) a (CropLeftBy c p) = do
    result <- render (w, h) a p
    let img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropLeft amt img
    return $ addCursorOffset (Location (-1 * c, 0)) $
             setImage cropped result
render (w, h) a (CropRightBy c p) = do
    result <- render (w, h) a p
    let img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropRight amt img
    -- xxx cursors
    return $ setImage cropped result
render (w, h) a (CropTopBy c p) = do
    result <- render (w, h) a p
    let img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropTop amt img
    return $ addCursorOffset (Location (0, -1 * c)) $
             setImage cropped result
render (w, h) a (CropBottomBy c p) = do
    result <- render (w, h) a p
    let img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropBottom amt img
    -- xxx crop cursors
    return $ setImage cropped result
render (w, h) c (HPad ch) = return $ setImage (charFill (c^.attr) ch w (max 1 h)) def
render (w, h) c (VPad ch) = return $ setImage (charFill (c^.attr) ch (max 1 w) h) def
render (w, h) c (HFill ch) = return $ setImage (charFill (c^.attr) ch w (min h 1)) def
render (w, h) c (VFill ch) = return $ setImage (charFill (c^.attr) ch (min w 1) h) def
render (_, h) c (HLimit w p) =
    -- xxx crop cursors
    render (w, h) c p
render (w, _) c (VLimit h p) =
    -- xxx crop cursors
    render (w, h) c p
render (_, h) c (HRelease p) = render (unrestricted, h) c p --- NB
render (w, _) c (VRelease p) = render (w, unrestricted) c p --- NB
render (w, h) c (UseAttr a p) = render (w, h) (c & attr .~ a) p
render (w, h) c (Translate tw th p) = do
    result <- render (w, h) c p
    let img = image result
    return $ addCursorOffset (Location (tw, th)) $
             setImage (crop w h $ translate tw th img) result
render sz c (ShowCursor n loc p) = do
    result <- render sz c p
    return $ result { cursors = (CursorLocation loc (Just n)):cursors result }
render sz c (SetSize sizeSetter p) = do
    result <- render sz c p
    let img = image result
        imgSz = (imageWidth img, imageHeight img)
    modify (sizeSetter imgSz)
    return result
render (w, h) c (HBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render (w, h) c prim) his

    let remainingWidth = w - (sum $ (imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (imageHeight . image . snd) <$> renderedHis

    renderedLows <- mapM (\(i, (prim, _)) -> (i,) <$> render (widthPerLow + (if i == 0 then padFirst else 0), heightPerLow) c prim) lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allWidths = imageWidth <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (offWidth, 0)
                offWidth = sum $ take i allWidths
            in cursors $ addCursorOffset off result

    return $ Render (horizCat allImages) (concat allTranslatedCursors)

render (w, h) c (VBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render (w, h) c prim) his

    let remainingHeight = h - (sum $ (imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (imageWidth . image . snd) <$> renderedHis

    renderedLows <- mapM (\(i, (prim, _)) -> (i,) <$> render (widthPerLow, heightPerLow + if i == 0 then padFirst else 0) c prim) lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allHeights = imageHeight <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (0, offHeight)
                offHeight = sum $ take i allHeights
            in cursors $ addCursorOffset off result

    return $ Render (vertCat allImages) (concat allTranslatedCursors)
