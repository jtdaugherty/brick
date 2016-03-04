{-# LANGUAGE BangPatterns #-}
module Brick.Widgets.Internal
  ( renderFinal
  , cropToContext
  , cropResultToContext
  )
where

import Control.Applicative
import Control.Lens ((^.), (&), (%~))
import Control.Monad.Trans.State.Lazy
import Control.Monad.Trans.Reader
import Data.Default
import qualified Graphics.Vty as V

import Brick.Types
import Brick.Types.Internal
import Brick.AttrMap

renderFinal :: AttrMap
            -> [Widget n]
            -> V.DisplayRegion
            -> ([CursorLocation n] -> Maybe (CursorLocation n))
            -> RenderState n
            -> (RenderState n, V.Picture, Maybe (CursorLocation n))
renderFinal aMap layerRenders sz chooseCursor rs = (newRS, pic, theCursor)
    where
        (layerResults, !newRS) = flip runState rs $ sequence $
            (\p -> runReaderT p ctx) <$>
            (render <$> cropToContext <$> layerRenders)
        ctx = Context def (fst sz) (snd sz) def aMap
        pic = V.picForLayers $ uncurry V.resize sz <$> (^.imageL) <$> layerResults
        layerCursors = (^.cursorsL) <$> layerResults
        theCursor = chooseCursor $ concat layerCursors

-- | After rendering the specified widget, crop its result image to the
-- dimensions in the rendering context.
cropToContext :: Widget n -> Widget n
cropToContext p =
    Widget (hSize p) (vSize p) (render p >>= cropResultToContext)

cropResultToContext :: Result n -> RenderM n (Result n)
cropResultToContext result = do
    c <- getContext
    return $ result & imageL %~ (V.crop (c^.availWidthL) (c^.availHeightL))
