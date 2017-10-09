{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
-- | This module provides the core widget combinators and rendering
-- routines. Everything this library does is in terms of these basic
-- primitives.
module Brick.Widgets.Core
  ( -- * Basic rendering primitives
    TextWidth(..)
  , emptyWidget
  , raw
  , txt
  , txtWrap
  , txtWrapWith
  , str
  , strWrap
  , strWrapWith
  , fill
  , hyperlink

  -- * Padding
  , padLeft
  , padRight
  , padTop
  , padBottom
  , padLeftRight
  , padTopBottom
  , padAll

  -- * Box layout
  , (<=>)
  , (<+>)
  , hBox
  , vBox

  -- * Limits
  , hLimit
  , vLimit
  , setAvailableSize

  -- * Attribute management
  , withDefAttr
  , modifyDefAttr
  , withAttr
  , forceAttr
  , overrideAttr
  , updateAttrMap

  -- * Border style management
  , withBorderStyle

  -- * Cursor placement
  , showCursor

  -- * Naming
  , Named(..)

  -- * Translation
  , translateBy

  -- * Cropping
  , cropLeftBy
  , cropRightBy
  , cropTopBy
  , cropBottomBy

  -- * Extent reporting
  , reportExtent
  , clickable

  -- * Scrollable viewports
  , viewport
  , visible
  , visibleRegion
  , unsafeLookupViewport
  , cached

  -- ** Adding offsets to cursor positions and visibility requests
  , addResultOffset

  -- ** Cropping results
  , cropToContext
  )
where

#if MIN_VERSION_base(4,8,0)
import Data.Monoid ((<>))
#else
import Control.Applicative
import Data.Monoid ((<>), mempty)
#endif

import Lens.Micro ((^.), (.~), (&), (%~), to, _1, _2, each, to, Lens')
import Lens.Micro.Mtl (use, (%=))
import Control.Monad ((>=>),when)
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.DList as DL
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Function as DF
import Data.List (sortBy, partition)
import qualified Graphics.Vty as V
import Control.DeepSeq

import Text.Wrap (wrapTextToLines, WrapSettings, defaultWrapSettings)

import Brick.Types
import Brick.Types.Internal
import Brick.Widgets.Border.Style
import Brick.Util (clOffset, clamp)
import Brick.AttrMap
import Brick.Widgets.Internal

-- | The class of text types that have widths measured in terminal
-- columns. NEVER use 'length' etc. to measure the length of a string if
-- you need to compute how much screen space it will occupy; always use
-- 'textWidth'.
class TextWidth a where
    textWidth :: a -> Int

instance TextWidth T.Text where
    textWidth = V.wcswidth . T.unpack

instance (F.Foldable f) => TextWidth (f Char) where
    textWidth = V.wcswidth . F.toList

-- | The class of types that store interface element names.
class Named a n where
    -- | Get the name of the specified value.
    getName :: a -> n

-- | When rendering the specified widget, use the specified border style
-- for any border rendering.
withBorderStyle :: BorderStyle -> Widget n -> Widget n
withBorderStyle bs p = Widget (hSize p) (vSize p) $ withReaderT (& ctxBorderStyleL .~ bs) (render p)

-- | The empty widget.
emptyWidget :: Widget n
emptyWidget = raw V.emptyImage

-- | Add an offset to all cursor locations, visbility requests, and
-- extents in the specified rendering result. This function is critical
-- for maintaining correctness in the rendering results as they are
-- processed successively by box layouts and other wrapping combinators,
-- since calls to this function result in converting from widget-local
-- coordinates to (ultimately) terminal-global ones so they can be
-- used by other combinators. You should call this any time you render
-- something and then translate it or otherwise offset it from its
-- original origin.
addResultOffset :: Location -> Result n -> Result n
addResultOffset off = addCursorOffset off .
                      addVisibilityOffset off .
                      addExtentOffset off

addVisibilityOffset :: Location -> Result n -> Result n
addVisibilityOffset off r = r & visibilityRequestsL.each.vrPositionL %~ (off <>)

addExtentOffset :: Location -> Result n -> Result n
addExtentOffset off r = r & extentsL.each %~ (\(Extent n l sz o) -> Extent n (off <> l) sz o)

-- | Render the specified widget and record its rendering extent using
-- the specified name (see also 'lookupExtent').
reportExtent :: n -> Widget n -> Widget n
reportExtent n p =
    Widget (hSize p) (vSize p) $ do
        result <- render p
        let ext = Extent n (Location (0, 0)) sz (Location (0, 0))
            sz = ( result^.imageL.to V.imageWidth
                 , result^.imageL.to V.imageHeight
                 )
        return $ result & extentsL %~ (ext:)

-- | Request mouse click events on the specified widget.
clickable :: n -> Widget n -> Widget n
clickable n p =
    Widget (hSize p) (vSize p) $ do
        clickableNamesL %= (n:)
        render $ reportExtent n p

addCursorOffset :: Location -> Result n -> Result n
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible l = l^.locationColumnL >= 0 && l^.locationRowL >= 0
    in r & cursorsL %~ (\cs -> onlyVisible $ (`clOffset` off) <$> cs)

unrestricted :: Int
unrestricted = 100000

-- | Take a substring capable of fitting into the number of specified
-- columns. This function takes character column widths into
-- consideration.
takeColumns :: Int -> String -> String
takeColumns _ "" = ""
takeColumns numCols (c:cs) =
    let w = V.safeWcwidth c
    in if w == numCols
       then [c]
       else if w < numCols
            then c : takeColumns (numCols - w) cs
            else ""

-- | Make a widget from a string, but wrap the words in the input's
-- lines at the available width using the default wrapping settings.
strWrap :: String -> Widget n
strWrap = strWrapWith defaultWrapSettings

-- | Make a widget from a string, but wrap the words in the input's
-- lines at the available width using the specified wrapping settings.
strWrapWith :: WrapSettings -> String -> Widget n
strWrapWith settings t = txtWrapWith settings $ T.pack t

safeTextWidth :: T.Text -> Int
safeTextWidth = V.safeWcswidth . T.unpack

-- | Make a widget from text, but wrap the words in the input's lines at
-- the available width using the default wrapping settings.
txtWrap :: T.Text -> Widget n
txtWrap = txtWrapWith defaultWrapSettings

-- | Make a widget from text, but wrap the words in the input's lines at
-- the available width using the specified wrapping settings.
txtWrapWith :: WrapSettings -> T.Text -> Widget n
txtWrapWith settings s =
    Widget Fixed Fixed $ do
      c <- getContext
      let theLines = fixEmpty <$> wrapTextToLines settings (c^.availWidthL) s
          fixEmpty l | T.null l = " "
                     | otherwise = l
      case force theLines of
          [] -> return emptyResult
          [one] -> return $ emptyResult & imageL .~ (V.text' (c^.attrL) one)
          multiple ->
              let maxLength = maximum $ safeTextWidth <$> multiple
                  lineImgs = lineImg <$> multiple
                  lineImg lStr = V.text' (c^.attrL)
                                   (lStr <> T.replicate (maxLength - safeTextWidth lStr) " ")
              in return $ emptyResult & imageL .~ (V.vertCat lineImgs)

-- | Build a widget from a 'String'. Breaks newlines up and space-pads
-- short lines out to the length of the longest line.
str :: String -> Widget n
str s =
    Widget Fixed Fixed $ do
      c <- getContext
      let theLines = fixEmpty <$> (dropUnused . lines) s
          fixEmpty :: String -> String
          fixEmpty [] = " "
          fixEmpty l = l
          dropUnused l = takeColumns (availWidth c) <$> take (availHeight c) l
      case force theLines of
          [] -> return emptyResult
          [one] -> return $ emptyResult & imageL .~ (V.string (c^.attrL) one)
          multiple ->
              let maxLength = maximum $ V.safeWcswidth <$> multiple
                  lineImgs = lineImg <$> multiple
                  lineImg lStr = V.string (c^.attrL) (lStr ++ replicate (maxLength - V.safeWcswidth lStr) ' ')
              in return $ emptyResult & imageL .~ (V.vertCat lineImgs)

-- | Build a widget from a 'T.Text' value. Behaves the same as 'str'
-- when the input contains multiple lines.
txt :: T.Text -> Widget n
txt = str . T.unpack

-- | Hyperlink the given widget to the specified URL. Not all terminal
-- emulators support this. In those that don't, this should have no
-- discernible effect.
hyperlink :: T.Text -> Widget n -> Widget n
hyperlink url p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        let attr = attrMapLookup (c^.ctxAttrNameL) (c^.ctxAttrMapL) `V.withURL` url
        withReaderT (& ctxAttrMapL %~ setDefaultAttr attr) (render p)

-- | Pad the specified widget on the left. If max padding is used, this
-- grows greedily horizontally; otherwise it defers to the padded
-- widget.
padLeft :: Padding -> Widget n -> Widget n
padLeft padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (hLimit i, hSize p)
    in Widget sz (vSize p) $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availWidthL
              Pad i -> c^.availWidthL - i
        result <- render $ hLimit lim p
        render $ (f $ vLimit (result^.imageL.to V.imageHeight) $ fill ' ') <+>
                 (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the right. If max padding is used,
-- this grows greedily horizontally; otherwise it defers to the padded
-- widget.
padRight :: Padding -> Widget n -> Widget n
padRight padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (hLimit i, hSize p)
    in Widget sz (vSize p) $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availWidthL
              Pad i -> c^.availWidthL - i
        result <- render $ hLimit lim p
        render $ (Widget Fixed Fixed $ return result) <+>
                 (f $ vLimit (result^.imageL.to V.imageHeight) $ fill ' ')

-- | Pad the specified widget on the top. If max padding is used, this
-- grows greedily vertically; otherwise it defers to the padded widget.
padTop :: Padding -> Widget n -> Widget n
padTop padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (vLimit i, vSize p)
    in Widget (hSize p) sz $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availHeightL
              Pad i -> c^.availHeightL - i
        result <- render $ vLimit lim p
        render $ (f $ hLimit (result^.imageL.to V.imageWidth) $ fill ' ') <=>
                 (Widget Fixed Fixed $ return result)

-- | Pad the specified widget on the bottom. If max padding is used,
-- this grows greedily vertically; otherwise it defers to the padded
-- widget.
padBottom :: Padding -> Widget n -> Widget n
padBottom padding p =
    let (f, sz) = case padding of
          Max -> (id, Greedy)
          Pad i -> (vLimit i, vSize p)
    in Widget (hSize p) sz $ do
        c <- getContext
        let lim = case padding of
              Max -> c^.availHeightL
              Pad i -> c^.availHeightL - i
        result <- render $ vLimit lim p
        render $ (Widget Fixed Fixed $ return result) <=>
                 (f $ hLimit (result^.imageL.to V.imageWidth) $ fill ' ')

-- | Pad a widget on the left and right. Defers to the padded widget for
-- growth policy.
padLeftRight :: Int -> Widget n -> Widget n
padLeftRight c w = padLeft (Pad c) $ padRight (Pad c) w

-- | Pad a widget on the top and bottom. Defers to the padded widget for
-- growth policy.
padTopBottom :: Int -> Widget n -> Widget n
padTopBottom r w = padTop (Pad r) $ padBottom (Pad r) w

-- | Pad a widget on all sides. Defers to the padded widget for growth
-- policy.
padAll :: Int -> Widget n -> Widget n
padAll v w = padLeftRight v $ padTopBottom v w

-- | Fill all available space with the specified character. Grows both
-- horizontally and vertically.
fill :: Char -> Widget n
fill ch =
    Widget Greedy Greedy $ do
      c <- getContext
      return $ emptyResult & imageL .~ (V.charFill (c^.attrL) ch (c^.availWidthL) (c^.availHeightL))

-- | Vertical box layout: put the specified widgets one above the other
-- in the specified order (uppermost first). Defers growth policies to
-- the growth policies of the contained widgets (if any are greedy, so
-- is the box).
vBox :: [Widget n] -> Widget n
vBox [] = emptyWidget
vBox pairs = renderBox vBoxRenderer pairs

-- | Horizontal box layout: put the specified widgets next to each other
-- in the specified order (leftmost first). Defers growth policies to
-- the growth policies of the contained widgets (if any are greedy, so
-- is the box).
hBox :: [Widget n] -> Widget n
hBox [] = emptyWidget
hBox pairs = renderBox hBoxRenderer pairs

-- | The process of rendering widgets in a box layout is exactly the
-- same except for the dimension under consideration (width vs. height),
-- in which case all of the same operations that consider one dimension
-- in the layout algorithm need to be switched to consider the other.
-- Because of this we fill a BoxRenderer with all of the functions
-- needed to consider the "primary" dimension (e.g. vertical if the
-- box layout is vertical) as well as the "secondary" dimension (e.g.
-- horizontal if the box layout is vertical). Doing this permits us to
-- have one implementation for box layout and parameterizing on the
-- orientation of all of the operations.
data BoxRenderer n =
    BoxRenderer { contextPrimary :: Lens' Context Int
                , contextSecondary :: Lens' Context Int
                , imagePrimary :: V.Image -> Int
                , imageSecondary :: V.Image -> Int
                , limitPrimary :: Int -> Widget n -> Widget n
                , limitSecondary :: Int -> Widget n -> Widget n
                , primaryWidgetSize :: Widget n -> Size
                , concatenatePrimary :: [V.Image] -> V.Image
                , locationFromOffset :: Int -> Location
                , padImageSecondary :: Int -> V.Image -> V.Attr -> V.Image
                }

vBoxRenderer :: BoxRenderer n
vBoxRenderer =
    BoxRenderer { contextPrimary = availHeightL
                , contextSecondary = availWidthL
                , imagePrimary = V.imageHeight
                , imageSecondary = V.imageWidth
                , limitPrimary = vLimit
                , limitSecondary = hLimit
                , primaryWidgetSize = vSize
                , concatenatePrimary = V.vertCat
                , locationFromOffset = Location . (0 ,)
                , padImageSecondary = \amt img a ->
                    let p = V.charFill a ' ' amt (V.imageHeight img)
                    in V.horizCat [img, p]
                }

hBoxRenderer :: BoxRenderer n
hBoxRenderer =
    BoxRenderer { contextPrimary = availWidthL
                , contextSecondary = availHeightL
                , imagePrimary = V.imageWidth
                , imageSecondary = V.imageHeight
                , limitPrimary = hLimit
                , limitSecondary = vLimit
                , primaryWidgetSize = hSize
                , concatenatePrimary = V.horizCat
                , locationFromOffset = Location . (, 0)
                , padImageSecondary = \amt img a ->
                    let p = V.charFill a ' ' (V.imageWidth img) amt
                    in V.vertCat [img, p]
                }

-- | Render a series of widgets in a box layout in the order given.
--
-- The growth policy of a box layout is the most unrestricted of the
-- growth policies of the widgets it contains, so to determine the hSize
-- and vSize of the box we just take the maximum (using the Ord instance
-- for Size) of all of the widgets to be rendered in the box.
--
-- Then the box layout algorithm proceeds as follows. We'll use
-- the vertical case to concretely describe the algorithm, but the
-- horizontal case can be envisioned just by exchanging all
-- "vertical"/"horizontal" and "rows"/"columns", etc., in the
-- description.
--
-- The growth policies of the child widgets determine the order in which
-- they are rendered, i.e., the order in which space in the box is
-- allocated to widgets as the algorithm proceeds. This is because order
-- matters: if we render greedy widgets first, there will be no space
-- left for non-greedy ones.
--
-- So we render all widgets with size 'Fixed' in the vertical dimension
-- first. Each is rendered with as much room as the overall box has, but
-- we assume that they will not be greedy and use it all. If they do,
-- maybe it's because the terminal is small and there just isn't enough
-- room to render everything.
--
-- Then the remaining height is distributed evenly amongst all remaining
-- (greedy) widgets and they are rendered in sub-boxes that are as high
-- as this even slice of rows and as wide as the box is permitted to be.
-- We only do this step at all if rendering the non-greedy widgets left
-- us any space, i.e., if there were any rows left.
--
-- After rendering the non-greedy and then greedy widgets, their images
-- are sorted so that they are stored in the order the original widgets
-- were given. All cursor locations and visibility requests in each
-- sub-widget are translated according to the position of the sub-widget
-- in the box.
--
-- All images are padded to be as wide as the widest sub-widget to
-- prevent attribute over-runs. Without this step the attribute used by
-- a sub-widget may continue on in an undesirable fashion until it hits
-- something with a different attribute. To prevent this and to behave
-- in the least surprising way, we pad the image on the right with
-- whitespace using the context's current attribute.
--
-- Finally, the padded images are concatenated together vertically and
-- returned along with the translated cursor positions and visibility
-- requests.
renderBox :: BoxRenderer n -> [Widget n] -> Widget n
renderBox br ws =
    Widget (maximum $ hSize <$> ws) (maximum $ vSize <$> ws) $ do
      c <- getContext

      let pairsIndexed = zip [(0::Int)..] ws
          (his, lows) = partition (\p -> (primaryWidgetSize br $ snd p) == Fixed)
                        pairsIndexed

      let availPrimary = c^.(contextPrimary br)
          availSecondary = c^.(contextSecondary br)

          renderHis _ prev [] = return $ DL.toList prev
          renderHis remainingPrimary prev ((i, prim):rest) = do
              result <- render $ limitPrimary br remainingPrimary
                               $ limitSecondary br availSecondary
                               $ cropToContext prim
              renderHis (remainingPrimary - (result^.imageL.(to $ imagePrimary br)))
                        (DL.snoc prev (i, result)) rest

      renderedHis <- renderHis availPrimary DL.empty his

      renderedLows <- case lows of
          [] -> return []
          ls -> do
              let remainingPrimary = c^.(contextPrimary br) -
                                     (sum $ (^._2.imageL.(to $ imagePrimary br)) <$> renderedHis)
                  primaryPerLow = remainingPrimary `div` length ls
                  rest = remainingPrimary - (primaryPerLow * length ls)
                  secondaryPerLow = c^.(contextSecondary br)
                  primaries = replicate rest (primaryPerLow + 1) <>
                              replicate (length ls - rest) primaryPerLow

              let renderLow ((i, prim), pri) =
                      (i,) <$> (render $ limitPrimary br pri
                                       $ limitSecondary br secondaryPerLow
                                       $ cropToContext prim)

              if remainingPrimary > 0 then mapM renderLow (zip ls primaries) else return []

      let rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
          allResults = snd <$> rendered
          allImages = (^.imageL) <$> allResults
          allPrimaries = imagePrimary br <$> allImages
          allTranslatedResults = (flip map) (zip [0..] allResults) $ \(i, result) ->
              let off = locationFromOffset br offPrimary
                  offPrimary = sum $ take i allPrimaries
              in addResultOffset off result
          -- Determine the secondary dimension value to pad to. In a
          -- vertical box we want all images to be the same width to
          -- avoid attribute over-runs or blank spaces with the wrong
          -- attribute. In a horizontal box we want all images to have
          -- the same height for the same reason.
          maxSecondary = maximum $ imageSecondary br <$> allImages
          padImage img = padImageSecondary br (maxSecondary - imageSecondary br img)
                         img (c^.attrL)
          paddedImages = padImage <$> allImages

      cropResultToContext $ Result (concatenatePrimary br paddedImages)
                            (concat $ cursors <$> allTranslatedResults)
                            (concat $ visibilityRequests <$> allTranslatedResults)
                            (concat $ extents <$> allTranslatedResults)

-- | Limit the space available to the specified widget to the specified
-- number of columns. This is important for constraining the horizontal
-- growth of otherwise-greedy widgets. This is non-greedy horizontally
-- and defers to the limited widget vertically.
hLimit :: Int -> Widget n -> Widget n
hLimit w p =
    Widget Fixed (vSize p) $
      withReaderT (& availWidthL .~ w) $ render $ cropToContext p

-- | Limit the space available to the specified widget to the specified
-- number of rows. This is important for constraining the vertical
-- growth of otherwise-greedy widgets. This is non-greedy vertically and
-- defers to the limited widget horizontally.
vLimit :: Int -> Widget n -> Widget n
vLimit h p =
    Widget (hSize p) Fixed $
      withReaderT (& availHeightL .~ h) $ render $ cropToContext p

-- | Set the rendering context height and width for this widget. This
-- is useful for relaxing the rendering size constraints on e.g. layer
-- widgets where cropping to the screen size is undesirable.
setAvailableSize :: (Int, Int) -> Widget n -> Widget n
setAvailableSize (w, h) p =
    Widget Fixed Fixed $
      withReaderT (\c -> c & availHeightL .~ h & availWidthL .~ w) $
        render $ cropToContext p

-- | When drawing the specified widget, set the current attribute used
-- for drawing to the one with the specified name. Note that the widget
-- may use further calls to 'withAttr' to override this; if you really
-- want to prevent that, use 'forceAttr'. Attributes used this way still
-- get merged hierarchically and still fall back to the attribute map's
-- default attribute. If you want to change the default attribute, use
-- 'withDefAttr'.
withAttr :: AttrName -> Widget n -> Widget n
withAttr an p =
    Widget (hSize p) (vSize p) $
      withReaderT (& ctxAttrNameL .~ an) (render p)

-- | Update the attribute map while rendering the specified widget: set
-- its new default attribute to the one that we get by looking up the
-- specified attribute name in the map and then modifying it with the
-- specified function.
modifyDefAttr :: (V.Attr -> V.Attr) -> Widget n -> Widget n
modifyDefAttr f p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrMapL %~ (setDefaultAttr (f $ getDefaultAttr (c^.ctxAttrMapL)))) (render p)

-- | Update the attribute map while rendering the specified widget: set
-- its new default attribute to the one that we get by looking up the
-- specified attribute name in the map.
withDefAttr :: AttrName -> Widget n -> Widget n
withDefAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrMapL %~ (setDefaultAttr (attrMapLookup an (c^.ctxAttrMapL)))) (render p)

-- | When rendering the specified widget, update the attribute map with
-- the specified transformation.
updateAttrMap :: (AttrMap -> AttrMap) -> Widget n -> Widget n
updateAttrMap f p =
    Widget (hSize p) (vSize p) $
        withReaderT (& ctxAttrMapL %~ f) (render p)

-- | When rendering the specified widget, force all attribute lookups
-- in the attribute map to use the value currently assigned to the
-- specified attribute name.
forceAttr :: AttrName -> Widget n -> Widget n
forceAttr an p =
    Widget (hSize p) (vSize p) $ do
        c <- getContext
        withReaderT (& ctxAttrMapL .~ (forceAttrMap (attrMapLookup an (c^.ctxAttrMapL)))) (render p)

-- | Override the lookup of 'targetName' to return the attribute value
-- associated with 'fromName' when rendering the specified widget.
-- See also 'mapAttrName'.
overrideAttr :: AttrName -> AttrName -> Widget n -> Widget n
overrideAttr targetName fromName =
    updateAttrMap (mapAttrName fromName targetName)

-- | Build a widget directly from a raw Vty image.
raw :: V.Image -> Widget n
raw img = Widget Fixed Fixed $ return $ emptyResult & imageL .~ img

-- | Translate the specified widget by the specified offset amount.
-- Defers to the translated widget for growth policy.
translateBy :: Location -> Widget n -> Widget n
translateBy off p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ addResultOffset off
             $ result & imageL %~ (V.translate (off^.locationColumnL) (off^.locationRowL))

-- | Crop the specified widget on the left by the specified number of
-- columns. Defers to the cropped widget for growth policy.
cropLeftBy :: Int -> Widget n -> Widget n
cropLeftBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.imageL) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropLeft amt img
      return $ addResultOffset (Location (-1 * cols, 0))
             $ result & imageL %~ cropped

-- | Crop the specified widget on the right by the specified number of
-- columns. Defers to the cropped widget for growth policy.
cropRightBy :: Int -> Widget n -> Widget n
cropRightBy cols p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageWidth (result^.imageL) - cols
          cropped img = if amt < 0 then V.emptyImage else V.cropRight amt img
      return $ result & imageL %~ cropped

-- | Crop the specified widget on the top by the specified number of
-- rows. Defers to the cropped widget for growth policy.
cropTopBy :: Int -> Widget n -> Widget n
cropTopBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.imageL) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropTop amt img
      return $ addResultOffset (Location (0, -1 * rows))
             $ result & imageL %~ cropped

-- | Crop the specified widget on the bottom by the specified number of
-- rows. Defers to the cropped widget for growth policy.
cropBottomBy :: Int -> Widget n -> Widget n
cropBottomBy rows p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let amt = V.imageHeight (result^.imageL) - rows
          cropped img = if amt < 0 then V.emptyImage else V.cropBottom amt img
      return $ result & imageL %~ cropped

-- | When rendering the specified widget, also register a cursor
-- positioning request using the specified name and location.
showCursor :: n -> Location -> Widget n -> Widget n
showCursor n cloc p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      return $ result & cursorsL %~ (CursorLocation cloc (Just n):)

hRelease :: Widget n -> Maybe (Widget n)
hRelease p =
    case hSize p of
        Fixed -> Just $ Widget Greedy (vSize p) $
                        withReaderT (& availWidthL .~ unrestricted) (render p)
        Greedy -> Nothing

vRelease :: Widget n -> Maybe (Widget n)
vRelease p =
    case vSize p of
        Fixed -> Just $ Widget (hSize p) Greedy $
                        withReaderT (& availHeightL .~ unrestricted) (render p)
        Greedy -> Nothing

-- | Render the specified widget. If the widget has an entry in the
-- rendering cache using the specified name as the cache key, use the
-- rendered version from the cache instead. If not, render the widget
-- and update the cache.
--
-- See also 'invalidateCacheEntry'.
cached :: (Ord n) => n -> Widget n -> Widget n
cached n w =
    Widget (hSize w) (vSize w) $ do
        result <- cacheLookup n
        case result of
            Just prevResult -> return prevResult
            Nothing  -> do
                wResult <- render w
                cacheUpdate n wResult
                return wResult

cacheLookup :: (Ord n) => n -> RenderM n (Maybe (Result n))
cacheLookup n = do
    cache <- lift $ gets (^.renderCacheL)
    return $ M.lookup n cache

cacheUpdate :: (Ord n) => n -> Result n -> RenderM n ()
cacheUpdate n r = lift $ modify (& renderCacheL %~ M.insert n r)

-- | Render the specified widget in a named viewport with the
-- specified type. This permits widgets to be scrolled without being
-- scrolling-aware. To make the most use of viewports, the specified
-- widget should use the 'visible' combinator to make a "visibility
-- request". This viewport combinator will then translate the resulting
-- rendering to make the requested region visible. In addition, the
-- 'Brick.Main.EventM' monad provides primitives to scroll viewports
-- created by this function if 'visible' is not what you want.
--
-- If a viewport receives more than one visibility request, then the
-- visibility requests are merged with the inner visibility request
-- taking preference. If a viewport receives more than one scrolling
-- request from 'Brick.Main.EventM', all are honored in the order in
-- which they are received.
viewport :: (Ord n, Show n)
         => n
         -- ^ The name of the viewport (must be unique and stable for
         -- reliable behavior)
         -> ViewportType
         -- ^ The type of viewport (indicates the permitted scrolling
         -- direction)
         -> Widget n
         -- ^ The widget to be rendered in the scrollable viewport
         -> Widget n
viewport vpname typ p =
    Widget Greedy Greedy $ do
      -- First, update the viewport size.
      c <- getContext
      let newVp = VP 0 0 newSize
          newSize = (c^.availWidthL, c^.availHeightL)
          doInsert (Just vp) = Just $ vp & vpSize .~ newSize
          doInsert Nothing = Just newVp

      let observeName :: (Ord n, Show n) => n -> RenderM n ()
          observeName n = do
              observed <- use observedNamesL
              case S.member n observed of
                  False -> observedNamesL %= S.insert n
                  True ->
                      error $ "Error: while rendering the interface, the name " <> show n <>
                              " was seen more than once. You should ensure that all of the widgets " <>
                              "in each interface have unique name values. This means either " <>
                              "using a different name type or adding constructors to your " <>
                              "existing one and using those to name your widgets.  For more " <>
                              "information, see the \"Resource Names\" section of the Brick User Guide."

      observeName vpname

      lift $ modify (& viewportMapL %~ (M.alter doInsert vpname))

      -- Then render the sub-rendering with the rendering layout
      -- constraint released (but raise an exception if we are asked to
      -- render an infinitely-sized widget in the viewport's scrolling
      -- dimension)
      let release = case typ of
            Vertical -> vRelease
            Horizontal -> hRelease
            Both -> vRelease >=> hRelease
          released = case release p of
            Just w -> w
            Nothing -> case typ of
                Vertical -> error $ "tried to embed an infinite-height " <>
                                    "widget in vertical viewport " <> (show vpname)
                Horizontal -> error $ "tried to embed an infinite-width " <>
                                      "widget in horizontal viewport " <> (show vpname)
                Both -> error $ "tried to embed an infinite-width or " <>
                                "infinite-height widget in 'Both' type " <>
                                "viewport " <> (show vpname)

      initialResult <- render released

      -- If the rendering state includes any scrolling requests for this
      -- viewport, apply those
      reqs <- lift $ gets $ (^.rsScrollRequestsL)
      let relevantRequests = snd <$> filter (\(n, _) -> n == vpname) reqs
      when (not $ null relevantRequests) $ do
          Just vp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
          let updatedVp = applyRequests relevantRequests vp
              applyRequests [] v = v
              applyRequests (rq:rqs) v =
                  case typ of
                      Horizontal -> scrollTo typ rq (initialResult^.imageL) $ applyRequests rqs v
                      Vertical -> scrollTo typ rq (initialResult^.imageL) $ applyRequests rqs v
                      Both -> scrollTo Horizontal rq (initialResult^.imageL) $
                              scrollTo Vertical rq (initialResult^.imageL) $
                              applyRequests rqs v
          lift $ modify (& viewportMapL %~ (M.insert vpname updatedVp))
          return ()

      -- If the sub-rendering requested visibility, update the scroll
      -- state accordingly
      when (not $ null $ initialResult^.visibilityRequestsL) $ do
          Just vp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
          let rqs = initialResult^.visibilityRequestsL
              updateVp vp' rq = case typ of
                  Both -> scrollToView Horizontal rq $ scrollToView Vertical rq vp'
                  Horizontal -> scrollToView typ rq vp'
                  Vertical -> scrollToView typ rq vp'
          lift $ modify (& viewportMapL %~ (M.insert vpname $ foldl updateVp vp rqs))

      -- If the size of the rendering changes enough to make the
      -- viewport offsets invalid, reset them
      Just vp <- lift $ gets $ (^.viewportMapL.to (M.lookup vpname))
      let img = initialResult^.imageL
          fixTop v = if V.imageHeight img < v^.vpSize._2
                   then v & vpTop .~ 0
                   else v
          fixLeft v = if V.imageWidth img < v^.vpSize._1
                   then v & vpLeft .~ 0
                   else v
          updateVp = case typ of
              Both -> fixLeft . fixTop
              Horizontal -> fixLeft
              Vertical -> fixTop
      lift $ modify (& viewportMapL %~ (M.insert vpname (updateVp vp)))

      -- Get the viewport state now that it has been updated.
      Just vpFinal <- lift $ gets (M.lookup vpname . (^.viewportMapL))

      -- Then perform a translation of the sub-rendering to fit into the
      -- viewport
      translated <- render $ translateBy (Location (-1 * vpFinal^.vpLeft, -1 * vpFinal^.vpTop))
                           $ Widget Fixed Fixed $ return initialResult

      -- Return the translated result with the visibility requests
      -- discarded
      let translatedSize = ( translated^.imageL.to V.imageWidth
                           , translated^.imageL.to V.imageHeight
                           )
      case translatedSize of
          (0, 0) -> do
              let spaceFill = V.charFill (c^.attrL) ' ' (c^.availWidthL) (c^.availHeightL)
              return $ translated & imageL .~ spaceFill
                                  & visibilityRequestsL .~ mempty
                                  & extentsL .~ mempty
          _ -> render $ cropToContext
                      $ padBottom Max
                      $ padRight Max
                      $ Widget Fixed Fixed
                      $ return $ translated & visibilityRequestsL .~ mempty

-- | Given a name, obtain the viewport for that name by consulting the
-- viewport map in the rendering monad. NOTE! Some care must be taken
-- when calling this function, since it only returns useful values
-- after the viewport in question has been rendered. If you call this
-- function during rendering before a viewport has been rendered, you
-- may get nothing or you may get a stale version of the viewport. This
-- is because viewports are updated during rendering and the one you are
-- interested in may not have been rendered yet. So if you want to use
-- this, be sure you know what you are doing.
unsafeLookupViewport :: (Ord n) => n -> RenderM n (Maybe Viewport)
unsafeLookupViewport name = lift $ gets (M.lookup name . (^.viewportMapL))

scrollTo :: ViewportType -> ScrollRequest -> V.Image -> Viewport -> Viewport
scrollTo Both _ _ _ = error "BUG: called scrollTo on viewport type 'Both'"
scrollTo Vertical req img vp = vp & vpTop .~ newVStart
    where
        newVStart = clamp 0 (V.imageHeight img - vp^.vpSize._2) adjustedAmt
        adjustedAmt = case req of
            VScrollBy amt -> vp^.vpTop + amt
            VScrollPage Up -> vp^.vpTop - vp^.vpSize._2
            VScrollPage Down -> vp^.vpTop + vp^.vpSize._2
            VScrollToBeginning -> 0
            VScrollToEnd -> V.imageHeight img - vp^.vpSize._2
            SetTop i -> i
            _ -> vp^.vpTop
scrollTo Horizontal req img vp = vp & vpLeft .~ newHStart
    where
        newHStart = clamp 0 (V.imageWidth img - vp^.vpSize._1) adjustedAmt
        adjustedAmt = case req of
            HScrollBy amt -> vp^.vpLeft + amt
            HScrollPage Up -> vp^.vpLeft - vp^.vpSize._1
            HScrollPage Down -> vp^.vpLeft + vp^.vpSize._1
            HScrollToBeginning -> 0
            HScrollToEnd -> V.imageWidth img - vp^.vpSize._1
            SetLeft i -> i
            _ -> vp^.vpLeft

scrollToView :: ViewportType -> VisibilityRequest -> Viewport -> Viewport
scrollToView Both _ _ = error "BUG: called scrollToView on 'Both' type viewport"
scrollToView Vertical rq vp = vp & vpTop .~ newVStart
    where
        curStart = vp^.vpTop
        curEnd = curStart + vp^.vpSize._2
        reqStart = rq^.vrPositionL.locationRowL

        reqEnd = rq^.vrPositionL.locationRowL + rq^.vrSizeL._2
        newVStart :: Int
        newVStart = if reqStart < vStartEndVisible
                   then reqStart
                   else vStartEndVisible
        vStartEndVisible = if reqEnd < curEnd
                           then curStart
                           else curStart + (reqEnd - curEnd)
scrollToView Horizontal rq vp = vp & vpLeft .~ newHStart
    where
        curStart = vp^.vpLeft
        curEnd = curStart + vp^.vpSize._1
        reqStart = rq^.vrPositionL.locationColumnL

        reqEnd = rq^.vrPositionL.locationColumnL + rq^.vrSizeL._1
        newHStart :: Int
        newHStart = if reqStart < hStartEndVisible
                   then reqStart
                   else hStartEndVisible
        hStartEndVisible = if reqEnd < curEnd
                           then curStart
                           else curStart + (reqEnd - curEnd)

-- | Request that the specified widget be made visible when it is
-- rendered inside a viewport. This permits widgets (whose sizes and
-- positions cannot be known due to being embedded in arbitrary layouts)
-- to make a request for a parent viewport to locate them and scroll
-- enough to put them in view. This, together with 'viewport', is what
-- makes the text editor and list widgets possible without making them
-- deal with the details of scrolling state management.
--
-- This does nothing if not rendered in a viewport.
visible :: Widget n -> Widget n
visible p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      let imageSize = ( result^.imageL.to V.imageWidth
                      , result^.imageL.to V.imageHeight
                      )
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if imageSize^._1 > 0 && imageSize^._2 > 0
               then result & visibilityRequestsL %~ (VR (Location (0, 0)) imageSize :)
               else result

-- | Similar to 'visible', request that a region (with the specified
-- 'Location' as its origin and 'V.DisplayRegion' as its size) be made
-- visible when it is rendered inside a viewport. The 'Location' is
-- relative to the specified widget's upper-left corner of (0, 0).
--
-- This does nothing if not rendered in a viewport.
visibleRegion :: Location -> V.DisplayRegion -> Widget n -> Widget n
visibleRegion vrloc sz p =
    Widget (hSize p) (vSize p) $ do
      result <- render p
      -- The size of the image to be made visible in a viewport must have
      -- non-zero size in both dimensions.
      return $ if sz^._1 > 0 && sz^._2 > 0
               then result & visibilityRequestsL %~ (VR vrloc sz :)
               else result

-- | Horizontal box layout: put the specified widgets next to each other
-- in the specified order. Defers growth policies to the growth policies
-- of both widgets.  This operator is a binary version of 'hBox'.
(<+>) :: Widget n
      -- ^ Left
      -> Widget n
      -- ^ Right
      -> Widget n
(<+>) a b = hBox [a, b]

-- | Vertical box layout: put the specified widgets one above the other
-- in the specified order. Defers growth policies to the growth policies
-- of both widgets.  This operator is a binary version of 'vBox'.
(<=>) :: Widget n
      -- ^ Top
      -> Widget n
      -- ^ Bottom
      -> Widget n
(<=>) a b = vBox [a, b]
