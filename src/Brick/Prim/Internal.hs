module Brick.Prim.Internal
  ( Render(..)
  , renderFinal
  )
where

import Control.Applicative
import Control.Lens ((^.))
import Data.Default
import qualified Data.Function as DF
import Data.List (sortBy)
import Graphics.Vty

import Brick.Core (Name(..), Location(..), CursorLocation(..))
import Brick.Prim
import Brick.Util (clOffset, for)

data Render =
    Render { image :: !Image
           , cursors :: ![CursorLocation]
           , sizes :: ![(Name, DisplayRegion)]
           }
           deriving Show

instance Default Render where
    def = Render emptyImage [] []

renderFinal :: a
            -> [Prim a]
            -> DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> (Picture, Maybe CursorLocation, [(Name, DisplayRegion)])
renderFinal st layerPrims sz chooseCursor = (pic, theCursor, theSizes)
    where
        layerResults = render st sz defAttr <$> layerPrims
        pic = picForLayers $ uncurry resize sz <$> image <$> layerResults
        layerCursors = cursors <$> layerResults
        theCursor = chooseCursor $ concat layerCursors
        theSizes = concat $ sizes <$> layerResults

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

render :: a -> DisplayRegion -> Attr -> Prim a -> Render
render s sz a (WithState getter f) = render (s^.getter) sz a $ f $ s^.getter
render _ (w, h) a (Txt s) =
    if w > 0 && h > 0
    then setImage (crop w h $ string a s) def
    else def
render _ (w, h) _ (Raw img) =
    if w > 0 && h > 0
    then setImage (crop w h img) def
    else def
render s (w, h) a (CropLeftBy c p) =
    let result = render s (w, h) a p
        img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropLeft amt img
    in addCursorOffset (Location (-1 * c, 0)) $
       setImage cropped result
render s (w, h) a (CropRightBy c p) =
    let result = render s (w, h) a p
        img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropRight amt img
    -- xxx cursors
    in setImage cropped result
render s (w, h) a (CropTopBy c p) =
    let result = render s (w, h) a p
        img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropTop amt img
    in addCursorOffset (Location (0, -1 * c)) $
       setImage cropped result
render s (w, h) a (CropBottomBy c p) =
    let result = render s (w, h) a p
        img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropBottom amt img
    -- xxx crop cursors
    in setImage cropped result
render _ (w, h) a (HPad c) = setImage (charFill a c w (max 1 h)) def
render _ (w, h) a (VPad c) = setImage (charFill a c (max 1 w) h) def
render _ (w, h) a (HFill c) = setImage (charFill a c w (min h 1)) def
render _ (w, h) a (VFill c) = setImage (charFill a c (min w 1) h) def
render s (_, h) a (HLimit w p) =
    -- xxx crop cursors
    render s (w, h) a p
render s (w, _) a (VLimit h p) =
    -- xxx crop cursors
    render s (w, h) a p
render s (_, h) a (HRelease p) = render s (unrestricted, h) a p --- NB
render s (w, _) a (VRelease p) = render s (w, unrestricted) a p --- NB
render s (w, h) _ (UseAttr a p) = render s (w, h) a p
render s (w, h) a (Translate tw th p) =
    let result = render s (w, h) a p
        img = image result
    in addCursorOffset (Location (tw, th)) $
       setImage (crop w h $ translate tw th img) result
render s sz a (ShowCursor n loc p) =
    let result = render s sz a p
    in result { cursors = (CursorLocation loc (Just n)):cursors result }
render s sz a (GetSize name p) =
    let result = render s sz a p
        img = image result
        imgSz = (imageWidth img, imageHeight img)
    in result { sizes = (name, imgSz) : sizes result
              }
render s (w, h) a (HBox pairs) =
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis = (\(i, (prim, _)) -> (i, render s (w, h) a prim)) <$> his
        remainingWidth = w - (sum $ (imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (imageHeight . image . snd) <$> renderedHis
        renderedLows = (\(i, (prim, _)) -> (i, render s (widthPerLow + (if i == 0 then padFirst else 0), heightPerLow) a prim)) <$> lows
        rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows

        allResults = snd <$> rendered
        allSizes = sizes <$> allResults
        allImages = image <$> allResults
        allWidths = imageWidth <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (offWidth, 0)
                offWidth = sum $ take i allWidths
            in cursors $ addCursorOffset off result

    in Render (horizCat allImages) (concat allTranslatedCursors) (concat allSizes)

render s (w, h) a (VBox pairs) =
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis = (\(i, (prim, _)) -> (i, render s (w, h) a prim)) <$> his
        remainingHeight = h - (sum $ (imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (imageWidth . image . snd) <$> renderedHis
        renderedLows = (\(i, (prim, _)) -> (i, render s (widthPerLow, heightPerLow + if i == 0 then padFirst else 0) a prim)) <$> lows
        rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows

        allResults = snd <$> rendered
        allSizes = sizes <$> allResults
        allImages = image <$> allResults
        allHeights = imageHeight <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (0, offHeight)
                offHeight = sum $ take i allHeights
            in cursors $ addCursorOffset off result

    in Render (vertCat allImages) (concat allTranslatedCursors) (concat allSizes)
