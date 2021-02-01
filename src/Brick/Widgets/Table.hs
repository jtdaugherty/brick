module Brick.Widgets.Table
  ( table
  )
where

import Control.Monad (forM)
import Data.List (transpose, intersperse)
import Graphics.Vty (imageHeight, imageWidth)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Border

table :: [[Widget n]] -> Widget n
table [] = emptyWidget
table rows =
    joinBorders $ border $ Widget Fixed Fixed $ do
        cellResults <- forM rows $ mapM render
        let rowHeights = rowHeight <$> cellResults
            colWidths = colWidth <$> byColumn
            rowHeight = maximum . fmap (imageHeight . image)
            colWidth = maximum . fmap (imageWidth . image)
            byColumn = transpose cellResults
            toW = Widget Fixed Fixed . return
            totalHeight = sum rowHeights
            mkColumn (width, colCells) = do
                paddedCells <- forM (zip rowHeights colCells) $ \(height, cell) ->
                    render $ padBottom (Pad (height - (imageHeight $ image cell)))
                        (toW cell)
                render $ vBox $ intersperse (hLimit width hBorder) $
                    toW <$> paddedCells
        columns <- mapM mkColumn $ zip colWidths byColumn
        render $ hBox $
            intersperse (vLimit (totalHeight + (length rows - 1)) vBorder) $
            toW <$> columns
