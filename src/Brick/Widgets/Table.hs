-- | Support for basic table drawing.
module Brick.Widgets.Table
  ( Table
  , ColumnAlignment(..)
  -- * Construction
  , table

  -- * Configuration
  , alignLeft
  , alignRight
  , alignCenter
  , setAlignment
  , surroundingBorder
  , rowBorders
  , columnBorders

  -- * Rendering
  , renderTable
  )
where

import Control.Monad (forM)
import Data.List (transpose, intersperse)
import qualified Data.Map as M
import Graphics.Vty (imageHeight, imageWidth)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

-- | Column alignment modes.
data ColumnAlignment =
    AlignLeft
    -- ^ Align all cells to the left (the default).
    | AlignCenter
    -- ^ Center the content in all cells in the column.
    | AlignRight
    -- ^ Align all cells to the right.
    deriving (Eq, Show, Read)

-- | A table data structure.
data Table n =
    Table { columnAlignments :: M.Map Int ColumnAlignment
          , tableRows :: [[Widget n]]
          , drawSurroundingBorder :: Bool
          , drawRowBorders :: Bool
          , drawColumnBorders :: Bool
          }

-- | Construct a new table.
--
-- The argument is the list of rows, with each element of the argument
-- list being the columns of the respective row.
--
-- By default, all columns are left-aligned. Use the alignment functions
-- in this module to change that behavior.
--
-- By default, the table will draw borders between columns, between
-- rows, and around the outside of the table. Border-drawing behavior
-- can be configured with the API in this module. Note that tables
-- always draw with 'joinBorders' enabled.
--
-- All cells of all rows MUST use the 'Fixed' growth policy for both
-- horizontal and vertical growth. If the argument list contains any
-- cells that use the 'Greedy' policy, this will throw an exception.
table :: [[Widget n]] -> Table n
table rows =
    if not allFixed
    then error "table: all cells must have Fixed horizontal and vertical growth policies"
    else t
    where
        allFixed = all fixedRow rows
        fixedRow = all fixedCell
        fixedCell w = hSize w == Fixed && vSize w == Fixed
        t = Table { columnAlignments = mempty
                  , tableRows = rows
                  , drawSurroundingBorder = True
                  , drawRowBorders = True
                  , drawColumnBorders = True
                  }

-- | Configure whether the table draws a border on its exterior.
surroundingBorder :: Bool -> Table n -> Table n
surroundingBorder b t =
    t { drawSurroundingBorder = b }

-- | Configure whether the table draws borders between its rows.
rowBorders :: Bool -> Table n -> Table n
rowBorders b t =
    t { drawRowBorders = b }

-- | Configure whether the table draws borders between its columns.
columnBorders :: Bool -> Table n -> Table n
columnBorders b t =
    t { drawColumnBorders = b }

-- | Align the specified column to the right. The argument is the column
-- index, starting with zero.
alignRight :: Int -> Table n -> Table n
alignRight = setAlignment AlignRight

-- | Align the specified column to the left. The argument is the column
-- index, starting with zero.
alignLeft :: Int -> Table n -> Table n
alignLeft = setAlignment AlignLeft

-- | Align the specified column to center. The argument is the column
-- index, starting with zero.
alignCenter :: Int -> Table n -> Table n
alignCenter = setAlignment AlignCenter

-- | Set the alignment for the specified column index (starting at
-- zero).
setAlignment :: ColumnAlignment -> Int -> Table n -> Table n
setAlignment a col t =
    t { columnAlignments = M.insert col a (columnAlignments t) }

-- | Render the table.
renderTable :: Table n -> Widget n
renderTable t =
    joinBorders $
    (if drawSurroundingBorder t then border else id) $
    Widget Fixed Fixed $ do
        let rows = tableRows t
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
            mkColumn (colIdx, width, colCells) = do
                let align = M.findWithDefault AlignLeft colIdx (columnAlignments t)
                paddedCells <- forM (zip rowHeights colCells) $ \(height, cell) ->
                    render $ maybeAlign align width $
                        padBottom (Pad (height - (imageHeight $ image cell)))
                        (toW cell)
                let maybeRowBorders = if drawRowBorders t
                                      then intersperse (hLimit width hBorder)
                                      else id
                render $ vBox $ maybeRowBorders $
                        toW <$> paddedCells
        columns <- mapM mkColumn $ zip3 [0..] colWidths byColumn
        let maybeColumnBorders =
                if drawColumnBorders t
                then let rowBorderHeight = if drawRowBorders t
                                           then length rows - 1
                                           else 0
                     in intersperse (vLimit (totalHeight + rowBorderHeight) vBorder)
                else id
        render $ hBox $ maybeColumnBorders $ toW <$> columns
