module Brick.Widgets.Table
  ( ColumnAlignment(..)
  , table
  )
where

import Control.Monad (forM)
import Data.List (transpose, intersperse)
import Graphics.Vty (imageHeight, imageWidth)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

data ColumnAlignment =
    AlignLeft
    | AlignCenter
    | AlignRight
    deriving (Eq, Show, Read)

table :: [ColumnAlignment] -> [[Widget n]] -> Widget n
table _ [] = emptyWidget
table aligns rows =
    joinBorders $ border $ Widget Fixed Fixed $ do
        cellResults <- forM rows $ mapM render
        let rowHeights = rowHeight <$> cellResults
            colWidths = colWidth <$> byColumn
            rowHeight = maximum . fmap (imageHeight . image)
            colWidth = maximum . fmap (imageWidth . image)
            byColumn = transpose cellResults
            toW = Widget Fixed Fixed . return
            totalHeight = sum rowHeights
            maybeAlign align width w =
                Widget Fixed Fixed $ do
                    result <- render w
                    case align of
                        AlignLeft -> return result
                        AlignCenter -> render $ hLimit width $ hCenter $ toW result
                        AlignRight -> render $
                                          padLeft (Pad (width - imageWidth (image result))) $
                                          toW result
            mkColumn (align, width, colCells) = do
                paddedCells <- forM (zip rowHeights colCells) $ \(height, cell) ->
                    render $ maybeAlign align width $
                        padBottom (Pad (height - (imageHeight $ image cell)))
                        (toW cell)
                render $ vBox $ intersperse (hLimit width hBorder) $
                    toW <$> paddedCells
        columns <- mapM mkColumn $ zip3 aligns colWidths byColumn
        render $ hBox $
            intersperse (vLimit (totalHeight + (length rows - 1)) vBorder) $
            toW <$> columns
