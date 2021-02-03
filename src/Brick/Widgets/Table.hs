-- | Support for basic table drawing.
module Brick.Widgets.Table
  (
  -- * Types
    Table
  , ColumnAlignment(..)
  , RowAlignment(..)
  , TableException(..)

  -- * Construction
  , table

  -- * Configuration
  , alignLeft
  , alignRight
  , alignCenter
  , alignTop
  , alignMiddle
  , alignBottom
  , setColAlignment
  , setRowAlignment
  , setDefaultColAlignment
  , setDefaultRowAlignment
  , surroundingBorder
  , rowBorders
  , columnBorders

  -- * Rendering
  , renderTable
  )
where

import Control.Monad (forM)
import qualified Control.Exception as E
import Data.List (transpose, intersperse, nub)
import qualified Data.Map as M
import Graphics.Vty (imageHeight, imageWidth)

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

-- | Column alignment modes.
data ColumnAlignment =
    AlignLeft
    -- ^ Align all cells to the left.
    | AlignCenter
    -- ^ Center the content horizontally in all cells in the column.
    | AlignRight
    -- ^ Align all cells to the right.
    deriving (Eq, Show, Read)

-- | Row alignment modes.
data RowAlignment =
    AlignTop
    -- ^ Align all cells to the top.
    | AlignMiddle
    -- ^ Center the content vertically in all cells in the row.
    | AlignBottom
    -- ^ Align all cells to the bottom.
    deriving (Eq, Show, Read)

-- | A table creation exception.
data TableException =
    TEUnequalRowSizes
    -- ^ Rows did not all have the same number of cells.
    | TEInvalidCellSizePolicy
    -- ^ Some cells in the table did not use the 'Fixed' size policy for
    -- both horizontal and vertical sizing.
    deriving (Eq, Show, Read)

instance E.Exception TableException where

-- | A table data structure.
data Table n =
    Table { columnAlignments :: M.Map Int ColumnAlignment
          , rowAlignments :: M.Map Int RowAlignment
          , tableRows :: [[Widget n]]
          , defaultColumnAlignment :: ColumnAlignment
          , defaultRowAlignment :: RowAlignment
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
-- By default, all rows are top-aligned. Use the alignment functions in
-- this module to change that behavior.
--
-- By default, the table will draw borders between columns, between
-- rows, and around the outside of the table. Border-drawing behavior
-- can be configured with the API in this module. Note that tables
-- always draw with 'joinBorders' enabled.
--
-- All cells of all rows MUST use the 'Fixed' growth policy for both
-- horizontal and vertical growth. If the argument list contains
-- any cells that use the 'Greedy' policy, this will raise a
-- 'TableException'.
--
-- All rows must have the same number of cells. If not, this will raise
-- a 'TableException'.
table :: [[Widget n]] -> Table n
table rows =
    if not allFixed
    then E.throw TEInvalidCellSizePolicy
    else if not allSameLength
         then E.throw TEUnequalRowSizes
         else t
    where
        allSameLength = length (nub (length <$> rows)) <= 1
        allFixed = all fixedRow rows
        fixedRow = all fixedCell
        fixedCell w = hSize w == Fixed && vSize w == Fixed
        t = Table { columnAlignments = mempty
                  , rowAlignments = mempty
                  , tableRows = rows
                  , drawSurroundingBorder = True
                  , drawRowBorders = True
                  , drawColumnBorders = True
                  , defaultColumnAlignment = AlignLeft
                  , defaultRowAlignment = AlignTop
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
alignRight = setColAlignment AlignRight

-- | Align the specified column to the left. The argument is the column
-- index, starting with zero.
alignLeft :: Int -> Table n -> Table n
alignLeft = setColAlignment AlignLeft

-- | Align the specified column to center. The argument is the column
-- index, starting with zero.
alignCenter :: Int -> Table n -> Table n
alignCenter = setColAlignment AlignCenter

-- | Align the specified row to the top. The argument is the row index,
-- starting with zero.
alignTop :: Int -> Table n -> Table n
alignTop = setRowAlignment AlignTop

-- | Align the specified row to the middle. The argument is the row
-- index, starting with zero.
alignMiddle :: Int -> Table n -> Table n
alignMiddle = setRowAlignment AlignMiddle

-- | Align the specified row to bottom. The argument is the row index,
-- starting with zero.
alignBottom :: Int -> Table n -> Table n
alignBottom = setRowAlignment AlignBottom

-- | Set the alignment for the specified column index (starting at
-- zero).
setColAlignment :: ColumnAlignment -> Int -> Table n -> Table n
setColAlignment a col t =
    t { columnAlignments = M.insert col a (columnAlignments t) }

-- | Set the alignment for the specified row index (starting at
-- zero).
setRowAlignment :: RowAlignment -> Int -> Table n -> Table n
setRowAlignment a row t =
    t { rowAlignments = M.insert row a (rowAlignments t) }

-- | Set the default column alignment for columns with no explicitly
-- configured alignment.
setDefaultColAlignment :: ColumnAlignment -> Table n -> Table n
setDefaultColAlignment a t =
    t { defaultColumnAlignment = a }

-- | Set the default row alignment for rows with no explicitly
-- configured alignment.
setDefaultRowAlignment :: RowAlignment -> Table n -> Table n
setDefaultRowAlignment a t =
    t { defaultRowAlignment = a }

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
            allRowAligns = (\i -> M.findWithDefault (defaultRowAlignment t) i (rowAlignments t)) <$>
                           [0..length rowHeights - 1]
            allColAligns = (\i -> M.findWithDefault (defaultColumnAlignment t) i (columnAlignments t)) <$>
                           [0..length byColumn - 1]
            rowHeight = maximum . fmap (imageHeight . image)
            colWidth = maximum . fmap (imageWidth . image)
            byColumn = transpose cellResults
            toW = Widget Fixed Fixed . return
            totalHeight = sum rowHeights
            applyColAlignment align width w =
                Widget Fixed Fixed $ do
                    result <- render w
                    case align of
                        AlignLeft -> return result
                        AlignCenter -> render $ hLimit width $ hCenter $ toW result
                        AlignRight -> render $
                                          padLeft (Pad (width - imageWidth (image result))) $
                                          toW result
            applyRowAlignment rHeight align result =
                case align of
                 AlignTop -> toW result
                 AlignMiddle -> vLimit rHeight $ vCenter $ toW result
                 AlignBottom -> vLimit rHeight $ padTop Max $ toW result
            mkColumn (hAlign, width, colCells) = do
                let paddedCells = flip map (zip3 allRowAligns rowHeights colCells) $ \(vAlign, rHeight, cell) ->
                        applyColAlignment hAlign width $
                        applyRowAlignment rHeight vAlign cell
                    maybeRowBorders = if drawRowBorders t
                                      then intersperse (hLimit width hBorder)
                                      else id
                render $ vBox $ maybeRowBorders paddedCells
        columns <- mapM mkColumn $ zip3 allColAligns colWidths byColumn
        let maybeColumnBorders =
                if drawColumnBorders t
                then let rowBorderHeight = if drawRowBorders t
                                           then length rows - 1
                                           else 0
                     in intersperse (vLimit (totalHeight + rowBorderHeight) vBorder)
                else id
        render $ hBox $ maybeColumnBorders $ toW <$> columns
