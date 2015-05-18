{-# LANGUAGE BangPatterns #-}
module Brick.Prim
  ( Prim(..)
  , Priority(..)
  , Render(..)
  , renderFinal
  , (=>>), (<<=), (<=>)
  , (+>>), (<<+), (<+>)
  )
where

import Control.Applicative ((<$>))
import Data.Default
import Data.String (IsString(..))
import qualified Data.Function as DF
import Data.List (sortBy)
import Graphics.Vty
  ( DisplayRegion
  , Image
  , Attr
  , imageWidth
  , imageHeight
  , horizCat
  , vertCat
  , Picture(..)
  , defAttr
  , picForLayers
  , translate
  , charFill
  , emptyImage
  , string
  , resize
  , cropBottom, cropTop, cropLeft, cropRight, crop
  )

import Brick.Core (CursorLocation(..), Location(..), Name(..))
import Brick.Util (for, clOffset)

data Render =
    Render { image :: !Image
           , cursors :: ![CursorLocation]
           , sizes :: ![(Name, DisplayRegion)]
           }
           deriving Show

instance Default Render where
    def = Render emptyImage [] []

data Priority = High | Low
              deriving (Show, Eq)

data Prim = Txt !String
          | HPad !Char
          | VPad !Char
          | HFill !Char
          | VFill !Char
          | HBox ![(Prim, Priority)]
          | VBox ![(Prim, Priority)]
          | HLimit !Int !Prim
          | VLimit !Int !Prim
          | UseAttr !Attr !Prim
          | Raw !Image
          | Translate !Int !Int !Prim
          | CropLeftBy !Int !Prim
          | CropRightBy !Int !Prim
          | CropTopBy !Int !Prim
          | CropBottomBy !Int !Prim
          | ShowCursor !Name !Location !Prim
          | GetSize !Name !Prim
          | HRelease !Prim
          | VRelease !Prim
          deriving Show

instance IsString Prim where
    fromString = Txt

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

render :: DisplayRegion -> Attr -> Prim -> Render
render (w, h) a (Txt s) =
    if w > 0 && h > 0
    then setImage (crop w h $ string a s) def
    else def
render (w, h) _ (Raw img) =
    if w > 0 && h > 0
    then setImage (crop w h img) def
    else def
render (w, h) a (CropLeftBy c p) =
    let result = render (w, h) a p
        img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropLeft amt img
    in addCursorOffset (Location (-1 * c, 0)) $
       setImage cropped result
render (w, h) a (CropRightBy c p) =
    let result = render (w, h) a p
        img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropRight amt img
    -- xxx cursors
    in setImage cropped result
render (w, h) a (CropTopBy c p) =
    let result = render (w, h) a p
        img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropTop amt img
    in addCursorOffset (Location (0, -1 * c)) $
       setImage cropped result
render (w, h) a (CropBottomBy c p) =
    let result = render (w, h) a p
        img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropBottom amt img
    -- xxx crop cursors
    in setImage cropped result
render (w, h) a (HPad c) = setImage (charFill a c w (max 1 h)) def
render (w, h) a (VPad c) = setImage (charFill a c (max 1 w) h) def
render (w, h) a (HFill c) = setImage (charFill a c w (min h 1)) def
render (w, h) a (VFill c) = setImage (charFill a c (min w 1) h) def
render (_, h) a (HLimit w p) =
    -- xxx crop cursors
    render (w, h) a p
render (w, _) a (VLimit h p) =
    -- xxx crop cursors
    render (w, h) a p
render (_, h) a (HRelease p) = render (unrestricted, h) a p --- NB
render (w, _) a (VRelease p) = render (w, unrestricted) a p --- NB
render (w, h) _ (UseAttr a p) = render (w, h) a p
render (w, h) a (Translate tw th p) =
    let result = render (w, h) a p
        img = image result
    in addCursorOffset (Location (tw, th)) $
       setImage (crop w h $ translate tw th img) result
render sz a (ShowCursor n loc p) =
    let result = render sz a p
    in result { cursors = (CursorLocation loc (Just n)):cursors result }
render sz a (GetSize name p) =
    let result = render sz a p
        img = image result
        imgSz = (imageWidth img, imageHeight img)
    in result { sizes = (name, imgSz) : sizes result
              }
render (w, h) a (HBox pairs) =
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis = (\(i, (prim, _)) -> (i, render (w, h) a prim)) <$> his
        remainingWidth = w - (sum $ (imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        padFirst = if widthPerLow * length lows < remainingWidth
                   then remainingWidth - widthPerLow * length lows
                   else 0
        heightPerLow = maximum $ (imageHeight . image . snd) <$> renderedHis
        renderedLows = (\(i, (prim, _)) -> (i, render (widthPerLow + (if i == 0 then padFirst else 0), heightPerLow) a prim)) <$> lows
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

render (w, h) a (VBox pairs) =
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis = (\(i, (prim, _)) -> (i, render (w, h) a prim)) <$> his
        remainingHeight = h - (sum $ (imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        padFirst = if heightPerLow * length lows < remainingHeight
                   then remainingHeight - heightPerLow * length lows
                   else 0
        widthPerLow = maximum $ (imageWidth . image . snd) <$> renderedHis
        renderedLows = (\(i, (prim, _)) -> (i, render (widthPerLow, heightPerLow + if i == 0 then padFirst else 0) a prim)) <$> lows
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

renderFinal :: [Prim]
            -> DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> (Picture, Maybe CursorLocation, [(Name, DisplayRegion)])
renderFinal layerPrims sz chooseCursor = (pic, theCursor, theSizes)
    where
        layerResults = render sz defAttr <$> layerPrims
        pic = picForLayers $ uncurry resize sz <$> image <$> layerResults
        layerCursors = cursors <$> layerResults
        theCursor = chooseCursor $ concat layerCursors
        theSizes = concat $ sizes <$> layerResults

(<+>) :: Prim -> Prim -> Prim
(<+>) a b = HBox [(a, High), (b, High)]

(<<+) :: Prim -> Prim -> Prim
(<<+) a b = HBox [(a, High), (b, Low)]

(+>>) :: Prim -> Prim -> Prim
(+>>) a b = HBox [(a, Low), (b, High)]

(<=>) :: Prim -> Prim -> Prim
(<=>) a b = VBox [(a, High), (b, High)]

(<<=) :: Prim -> Prim -> Prim
(<<=) a b = VBox [(a, High), (b, Low)]

(=>>) :: Prim -> Prim -> Prim
(=>>) a b = VBox [(a, Low), (b, High)]
