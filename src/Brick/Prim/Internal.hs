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
render sz ctx (With target f) = do
    outerState <- get
    let innerPrim = f oldInnerState
        oldInnerState = outerState^.target
        (innerRender, newInnerState) = runState (render sz ctx innerPrim) oldInnerState
    target .= newInnerState
    return innerRender
render (w, h) ctx (Txt s) =
    return $ if w > 0 && h > 0
             then setImage (crop w h $ string (ctx^.attr) s) def
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
render (w, h) ctx (HPad c) = return $ setImage (charFill (ctx^.attr) c w (max 1 h)) def
render (w, h) ctx (VPad c) = return $ setImage (charFill (ctx^.attr) c (max 1 w) h) def
render (w, h) ctx (HFill c) = return $ setImage (charFill (ctx^.attr) c w (min h 1)) def
render (w, h) ctx (VFill c) = return $ setImage (charFill (ctx^.attr) c (min w 1) h) def
render (_, h) ctx (HLimit w p) =
    -- xxx crop cursors
    render (w, h) ctx p
render (w, _) ctx (VLimit h p) =
    -- xxx crop cursors
    render (w, h) ctx p
render (_, h) ctx (HRelease p) = render (unrestricted, h) ctx p --- NB
render (w, _) ctx (VRelease p) = render (w, unrestricted) ctx p --- NB
render (w, h) ctx (UseAttr a p) = render (w, h) (ctx & attr .~ a) p
render (w, h) ctx (Translate tw th p) = do
    result <- render (w, h) ctx p
    let img = image result
    return $ addCursorOffset (Location (tw, th)) $
             setImage (crop w h $ translate tw th img) result
render sz ctx (ShowCursor n loc p) = do
    result <- render sz ctx p
    return $ result { cursors = (CursorLocation loc (Just n)):cursors result }
render sz ctx (SetSize sizeSetter p) = do
    result <- render sz ctx p
    let img = image result
        imgSz = (imageWidth img, imageHeight img)
    modify (sizeSetter imgSz)
    return result
render (w, h) ctx (HBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render (w, h) ctx prim) his

    let remainingWidth = w - (sum $ (imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (imageHeight . image . snd) <$> renderedHis

    renderedLows <- mapM (\(i, (prim, _)) -> (i,) <$> render (widthPerLow + (if i == 0 then padFirst else 0), heightPerLow) ctx prim) lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allWidths = imageWidth <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (offWidth, 0)
                offWidth = sum $ take i allWidths
            in cursors $ addCursorOffset off result

    return $ Render (horizCat allImages) (concat allTranslatedCursors)

render (w, h) ctx (VBox pairs) = do
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed

    renderedHis <- mapM (\(i, (prim, _)) -> (i,) <$> render (w, h) ctx prim) his

    let remainingHeight = h - (sum $ (imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (imageWidth . image . snd) <$> renderedHis

    renderedLows <- mapM (\(i, (prim, _)) -> (i,) <$> render (widthPerLow, heightPerLow + if i == 0 then padFirst else 0) ctx prim) lows

    let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
        allResults = snd <$> rendered
        allImages = image <$> allResults
        allHeights = imageHeight <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (0, offHeight)
                offHeight = sum $ take i allHeights
            in cursors $ addCursorOffset off result

    return $ Render (vertCat allImages) (concat allTranslatedCursors)
