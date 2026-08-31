{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Brick.Widgets.Internal
  ( renderFinal
  , cropToContext
  , cropResultToContext
  , renderDynBorder
  , renderWidget
  )
where

import Lens.Micro ((^.), (&), (%~), (.~))
import Lens.Micro.Mtl ((%=))
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Reader
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Traversable as T
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Types.Internal
import Brick.AttrMap
import Brick.Widgets.Border.Style
import Brick.BorderMap (BorderMap)
import qualified Brick.BorderMap as BM

renderFinal :: (Ord n)
            => AttrMap
            -> [Widget n]
            -> V.DisplayRegion
            -> ([CursorLocation n] -> Maybe (CursorLocation n))
            -> RenderState n
            -> (RenderState n, V.Picture, Maybe (CursorLocation n), [Extent n])
renderFinal aMap layerRenders (w, h) chooseCursor rs =
    (newRS, picWithBg, theCursor, concat layerExtents)
    where
        -- Reset various fields from the last rendering state so they
        -- don't accumulate or affect this rendering.
        resetRs = rs & reportedExtentsL .~ mempty
                     & observedNamesL .~ mempty
                     & clickableNamesL .~ mempty

        (allLayers, !newRS) = flip runState resetRs $ do
            go $ Seq.fromList layerRenders
            where
                go layers =
                    case Seq.viewr layers of
                        Seq.EmptyR -> return mempty
                        rest Seq.:> next -> do
                            thisLayerResults <- flip runReaderT ctx $
                                processMainLayer next

                            restResults <- go rest
                            return $ restResults <> thisLayerResults

                processMainLayer layerWidget = do
                    let recordExtents r =
                            forM_ (r^.extentsL) $ \e ->
                                reportedExtentsL %= M.insert (extentName e) e

                    result <- translateResult <$> (render $ cropToContext layerWidget)
                    recordExtents result

                    let gatherLayer r = do
                            let r' = translateResult r
                            recordExtents r'

                            rest <- T.mapM gatherLayer $ r'^.extraLayersL
                            return $ concatSeq rest Seq.|> r'

                    translatedLayerResults <- T.mapM gatherLayer $ result^.extraLayersL
                    return $ concatSeq translatedLayerResults Seq.|> result

        translateResult r =
            let off = translationOffset r
            in addResultOffset off $
               r & imageL %~ (V.translate (off^.locationColumnL) (off^.locationRowL))

        concatSeq ss =
            F.foldr (Seq.><) Seq.empty ss

        ctx = Context { ctxAttrName = mempty
                      , availWidth = w
                      , availHeight = h
                      , windowWidth = w
                      , windowHeight = h
                      , ctxBorderStyle = defaultBorderStyle
                      , ctxAttrMap = aMap
                      , ctxDynBorders = False
                      , ctxVScrollBarOrientation = Nothing
                      , ctxVScrollBarRenderer = Nothing
                      , ctxHScrollBarOrientation = Nothing
                      , ctxHScrollBarRenderer = Nothing
                      , ctxHScrollBarShowHandles = False
                      , ctxVScrollBarShowHandles = False
                      , ctxHScrollBarClickableConstr = Nothing
                      , ctxVScrollBarClickableConstr = Nothing
                      }

        pic = V.picForLayers $ F.toList $ V.resize w h <$> (^.imageL) <$> allLayers

        -- picWithBg is a workaround for runaway attributes.
        -- See https://github.com/coreyoconnor/vty/issues/95
        picWithBg = pic { V.picBackground = V.Background ' ' V.defAttr }

        (layerCursors, layerExtents) = Seq.unzipWith layerInfo allLayers
        layerInfo l = (l^.cursorsL, l^.extentsL)
        theCursor = chooseCursor $ concat $ F.toList layerCursors

-- | After rendering the specified widget, crop its result image to the
-- dimensions in the rendering context.
cropToContext :: Widget n -> Widget n
cropToContext p =
    Widget (hSize p) (vSize p) (render p >>= cropResultToContext)

cropResultToContext :: Result n -> RenderM n (Result n)
cropResultToContext result = do
    c <- getContext
    return $ result & imageL   %~ cropImage   c
                    & cursorsL %~ cropCursors c
                    & extentsL %~ cropExtents c
                    & bordersL %~ cropBorders c

cropImage :: Context n -> V.Image -> V.Image
cropImage c = V.crop (max 0 $ c^.availWidthL) (max 0 $ c^.availHeightL)

cropCursors :: Context n -> [CursorLocation n] -> [CursorLocation n]
cropCursors ctx cs = mapMaybe cropCursor cs
    where
        -- A cursor location is removed if it is not within the region
        -- described by the context.
        cropCursor c | outOfContext c = Nothing
                     | otherwise      = Just c
        outOfContext c =
            or [ c^.cursorLocationL.locationRowL    < 0
               , c^.cursorLocationL.locationColumnL < 0
               , c^.cursorLocationL.locationRowL    >= ctx^.availHeightL
               , c^.cursorLocationL.locationColumnL >= ctx^.availWidthL
               ]

cropExtents :: Context n -> [Extent n] -> [Extent n]
cropExtents ctx es = mapMaybe cropExtent es
    where
        cropExtent (Extent n (Location (c, r)) (w, h)) =
            -- Clamp the original extent's UL corner to the context.
            --
            -- Clamp the original extent's LR corner to the context.
            --
            -- Keep the modified extent (i.e. with clamped corners)
            -- only if the resulting extent has non-zero size in both
            -- dimensions.
            let nonEmpty = nonEmptyH && nonEmptyV
                nonEmptyH = newWidth > 0
                nonEmptyV = newHeight > 0
                newWidth = newEndCol - newStartCol
                newHeight = newEndRow - newStartRow
                (newStartCol, newStartRow) = clampCorner (c, r)
                (newEndCol, newEndRow) = clampCorner (c + w, r + h)
                clampCorner (cols, rows) =
                    ( clampRange (ctx^.availWidthL) cols
                    , clampRange (ctx^.availHeightL) rows
                    )
                clampRange bound val =
                    min bound $ max 0 val
                newExtent = Extent n (Location (newStartCol, newStartRow)) (newWidth, newHeight)
            in if nonEmpty
               then Just newExtent
               else Nothing

cropBorders :: Context n -> BorderMap DynBorder -> BorderMap DynBorder
cropBorders ctx = BM.crop Edges
    { eTop = 0
    , eBottom = availHeight ctx - 1
    , eLeft = 0
    , eRight = availWidth ctx - 1
    }

renderDynBorder :: DynBorder -> V.Image
renderDynBorder db = V.char (dbAttr db) $ getBorderChar $ dbStyle db
    where
        getBorderChar = case bsDraw <$> dbSegments db of
            --    top   bot   left  right
            Edges False False False False -> const ' '
            Edges False False _     _     -> bsHorizontal
            Edges _     _     False False -> bsVertical
            Edges False True  False True  -> bsCornerTL
            Edges False True  True  False -> bsCornerTR
            Edges True  False False True  -> bsCornerBL
            Edges True  False True  False -> bsCornerBR
            Edges False True  True  True  -> bsIntersectT
            Edges True  False True  True  -> bsIntersectB
            Edges True  True  False True  -> bsIntersectL
            Edges True  True  True  False -> bsIntersectR
            Edges True  True  True  True  -> bsIntersectFull

-- | This function provides a simplified interface to rendering a list
-- of 'Widget's as a 'V.Picture' outside of the context of an 'App'.
-- This can be useful in a testing setting but isn't intended to be used
-- for normal application rendering. The API is deliberately narrower
-- than the main interactive API and is not yet stable. Use at your own
-- risk.
--
-- Consult the [Vty library documentation](https://hackage.haskell.org/package/vty)
-- for details on how to output the resulting 'V.Picture'.
renderWidget :: (Ord n)
             => Maybe AttrMap
             -- ^ Optional attribute map used to render. If omitted,
             -- an empty attribute map with the terminal's default
             -- attribute will be used.
             -> [Widget n]
             -- ^ The widget layers to render, topmost first.
             -> V.DisplayRegion
             -- ^ The size of the display region in which to render the
             -- layers.
             -> V.Picture
renderWidget mAttrMap layerRenders region = pic
    where
        initialRS = RS { viewportMap = M.empty
                       , rsScrollRequests = []
                       , observedNames = S.empty
                       , renderCache = mempty
                       , clickableNames = []
                       , requestedVisibleNames_ = S.empty
                       , reportedExtents = mempty
                       }
        am = fromMaybe (attrMap V.defAttr []) mAttrMap
        (_, pic, _, _) = renderFinal am layerRenders region (const Nothing) initialRS
