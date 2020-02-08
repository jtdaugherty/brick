{-# LANGUAGE BangPatterns #-}
module Brick.Widgets.Internal
  ( renderFinal
  , cropToContext
  , cropResultToContext
  , renderDynBorder
  )
where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Lens.Micro ((^.), (&), (%~))
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Data.Maybe (catMaybes)
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Types.Internal
import Brick.AttrMap
import Brick.Widgets.Border.Style
import Brick.BorderMap (BorderMap)
import qualified Brick.BorderMap as BM

renderFinal :: AttrMap
            -> [Widget n]
            -> V.DisplayRegion
            -> ([CursorLocation n] -> Maybe (CursorLocation n))
            -> RenderState n
            -> (RenderState n, V.Picture, Maybe (CursorLocation n), [Extent n])
renderFinal aMap layerRenders sz chooseCursor rs = (newRS, picWithBg, theCursor, concat layerExtents)
    where
        (layerResults, !newRS) = flip runState rs $ sequence $
            (\p -> runReaderT p ctx) <$>
            (render <$> cropToContext <$> layerRenders)
        ctx = Context mempty (fst sz) (snd sz) defaultBorderStyle aMap False
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.imageL) <$> layerResults
        -- picWithBg is a workaround for runaway attributes.
        -- See https://github.com/coreyoconnor/vty/issues/95
        picWithBg = pic { V.picBackground = V.Background ' ' V.defAttr }
        layerCursors = (^.cursorsL) <$> layerResults
        layerExtents = reverse $ (^.extentsL) <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

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

cropImage :: Context -> V.Image -> V.Image
cropImage c = V.crop (max 0 $ c^.availWidthL) (max 0 $ c^.availHeightL)

cropCursors :: Context -> [CursorLocation n] -> [CursorLocation n]
cropCursors ctx cs = catMaybes $ cropCursor <$> cs
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

cropExtents :: Context -> [Extent n] -> [Extent n]
cropExtents ctx es = catMaybes $ cropExtent <$> es
    where
        -- An extent is cropped in places where it is not within the
        -- region described by the context.
        --
        -- If its entirety is outside the context region, it is dropped.
        --
        -- Otherwise its size and upper left corner are adjusted so that
        -- they are contained within the context region.
        cropExtent (Extent n (Location (c, r)) (w, h) (Location (oC, oR))) =
            -- First, clamp the upper-left corner to at least (0, 0).
            let c' = max c 0
                r' = max r 0
                -- Compute deltas for the offset since if the upper-left
                -- corner moved, so should the offset.
                dc = c' - c
                dr = r' - r
                -- Then, determine the new lower-right corner based on
                -- the clamped corner.
                endCol = c' + w
                endRow = r' + h
                -- Then clamp the lower-right corner based on the
                -- context
                endCol' = min (ctx^.availWidthL) endCol
                endRow' = min (ctx^.availHeightL) endRow
                -- Then compute the new width and height from the
                -- clamped lower-right corner.
                w' = endCol' - c'
                h' = endRow' - r'
                e = Extent n (Location (c', r')) (w', h') (Location (oC + dc, oR + dr))
            in if w' < 0 || h' < 0
               then Nothing
               else Just e

cropBorders :: Context -> BorderMap DynBorder -> BorderMap DynBorder
cropBorders ctx = BM.crop Edges
    { eTop = 0
    , eBottom = availHeight ctx - 1
    , eLeft = 0
    , eRight = availWidth ctx - 1
    }

renderDynBorder :: DynBorder -> V.Image
renderDynBorder db = V.char (dbAttr db) . ($dbStyle db) $ case bsDraw <$> dbSegments db of
    --    top   bot   left  right
    Edges False False False False -> const ' ' -- dunno lol (but should never happen, so who cares)
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
