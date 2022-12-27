{-# LANGUAGE MultiWayIf #-}
-- | This module provides a table widget that can draw other widgets
-- in a table layout, draw borders between rows and columns, and allow
-- configuration of row and column alignment. To get started, see
-- 'table'.
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

  -- * Low-level API
  , RenderedTableCells(..)
  , BorderConfiguration(..)
  , tableCellLayout
  , addBorders
  , alignColumns
  )
where

import Control.Monad (forM)
import qualified Control.Exception as E
import Data.List (transpose, intersperse, nub)
import qualified Data.Map as M
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Graphics.Vty (imageHeight, imageWidth, charFill)
import Lens.Micro ((^.))

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border

-- | Column alignment modes. Use these modes with the alignment
-- functions in this module to configure column alignment behavior.
data ColumnAlignment =
    AlignLeft
    -- ^ Align all cells to the left.
    | AlignCenter
    -- ^ Center the content horizontally in all cells in the column.
    | AlignRight
    -- ^ Align all cells to the right.
    deriving (Eq, Show, Read)

-- | Row alignment modes. Use these modes with the alignment functions
-- in this module to configure row alignment behavior.
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

-- | A table data structure for widgets of type 'Widget' @n@. Create a
-- table with 'table'.
data Table n =
    Table { columnAlignments :: M.Map Int ColumnAlignment
          , rowAlignments :: M.Map Int RowAlignment
          , tableRows :: [[Widget n]]
          , defaultColumnAlignment :: ColumnAlignment
          , defaultRowAlignment :: RowAlignment
          , tableBorderConfiguration :: BorderConfiguration
          }

-- | A border configuration for a table.
data BorderConfiguration =
    BorderConfiguration { drawSurroundingBorder :: Bool
                        , drawRowBorders :: Bool
                        , drawColumnBorders :: Bool
                        }

-- | Construct a new table.
--
-- The argument is the list of rows with the topmost row first, with
-- each element of the argument list being the contents of the cells in
-- in each column of the respective row, with the leftmost cell first.
--
-- Each row's height is determined by the height of the tallest cell
-- in that row, and each column's width is determined by the width of
-- the widest cell in that column. This means that control over row
-- and column dimensions is a matter of controlling the size of the
-- individual cells, such as by wrapping cell contents in padding,
-- 'fill' and 'hLimit' or 'vLimit', etc. This also means that it is not
-- necessary to explicitly set the width of most table cells because
-- the table will determine the per-row and per-column dimensions by
-- looking at the largest cell contents. In particular, this means
-- that the table's alignment logic only has an effect when a given
-- cell's contents are smaller than the maximum for its row and column,
-- thus giving the table some way to pad the contents to result in the
-- desired alignment.
--
-- By default:
--
-- * All columns are left-aligned. Use the alignment functions in this
-- module to change that behavior.
-- * All rows are top-aligned. Use the alignment functions in this
-- module to change that behavior.
-- * The table will draw borders between columns, between rows, and
-- around the outside of the table. Border-drawing behavior can be
-- configured with the API in this module. Note that tables always draw
-- with 'joinBorders' enabled. If a cell's contents has smart borders
-- but you don't want those borders to connect to the surrounding table
-- borders, wrap the cell's contents with 'freezeBorders'.
--
-- All cells of all rows MUST use the 'Fixed' growth policy for both
-- horizontal and vertical growth. If the argument list contains any
-- cells that use the 'Greedy' policy, this function will raise a
-- 'TableException'.
--
-- All rows MUST have the same number of cells. If not, this function
-- will raise a 'TableException'.
table :: [[Widget n]] -> Table n
table rows =
    if | not allFixed      -> E.throw TEInvalidCellSizePolicy
       | not allSameLength -> E.throw TEUnequalRowSizes
       | otherwise         -> t
    where
        allSameLength = length (nub (length <$> rows)) <= 1
        allFixed = all fixedRow rows
        fixedRow = all fixedCell
        fixedCell w = hSize w == Fixed && vSize w == Fixed
        t = Table { columnAlignments = mempty
                  , rowAlignments = mempty
                  , tableRows = rows
                  , defaultColumnAlignment = AlignLeft
                  , defaultRowAlignment = AlignTop
                  , tableBorderConfiguration =
                      BorderConfiguration { drawSurroundingBorder = True
                                          , drawRowBorders = True
                                          , drawColumnBorders = True
                                          }
                  }

-- | Configure whether the table draws a border on its exterior.
surroundingBorder :: Bool -> Table n -> Table n
surroundingBorder b t =
    t { tableBorderConfiguration = (tableBorderConfiguration t) { drawSurroundingBorder = b } }

-- | Configure whether the table draws borders between its rows.
rowBorders :: Bool -> Table n -> Table n
rowBorders b t =
    t { tableBorderConfiguration = (tableBorderConfiguration t) { drawRowBorders = b } }

-- | Configure whether the table draws borders between its columns.
columnBorders :: Bool -> Table n -> Table n
columnBorders b t =
    t { tableBorderConfiguration = (tableBorderConfiguration t) { drawColumnBorders = b } }

-- | Align the specified column to the right. The argument is the column
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignRight :: Int -> Table n -> Table n
alignRight = setColAlignment AlignRight

-- | Align the specified column to the left. The argument is the column
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignLeft :: Int -> Table n -> Table n
alignLeft = setColAlignment AlignLeft

-- | Align the specified column to center. The argument is the column
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignCenter :: Int -> Table n -> Table n
alignCenter = setColAlignment AlignCenter

-- | Align the specified row to the top. The argument is the row index,
-- starting with zero. Silently does nothing if the index is out of
-- range.
alignTop :: Int -> Table n -> Table n
alignTop = setRowAlignment AlignTop

-- | Align the specified row to the middle. The argument is the row
-- index, starting with zero. Silently does nothing if the index is out
-- of range.
alignMiddle :: Int -> Table n -> Table n
alignMiddle = setRowAlignment AlignMiddle

-- | Align the specified row to bottom. The argument is the row index,
-- starting with zero. Silently does nothing if the index is out of
-- range.
alignBottom :: Int -> Table n -> Table n
alignBottom = setRowAlignment AlignBottom

-- | Set the alignment for the specified column index (starting at
-- zero). Silently does nothing if the index is out of range.
setColAlignment :: ColumnAlignment -> Int -> Table n -> Table n
setColAlignment a col t =
    t { columnAlignments = M.insert col a (columnAlignments t) }

-- | Set the alignment for the specified row index (starting at
-- zero). Silently does nothing if the index is out of range.
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
    Widget Fixed Fixed $ do
        tableCellLayout t >>= addBorders >>= render

-- | The result of performing table cell intermediate rendering and
-- layout.
data RenderedTableCells n =
    RenderedTableCells { renderedTableRows :: [[Widget n]]
                       -- ^ The table's cells in row-major order.
                       , renderedTableColumnWidths :: [Int]
                       -- ^ The widths of the table's columns.
                       , renderedTableRowHeights :: [Int]
                       -- ^ The heights of the table's rows.
                       , borderConfiguration :: BorderConfiguration
                       -- ^ The border configuration to use.
                       }

-- | Augment rendered table cells with borders according to the
-- border configuration accompanying the cells.
addBorders :: RenderedTableCells n -> RenderM n (Widget n)
addBorders r = do
    let cfg = borderConfiguration r
        rows = renderedTableRows r
        rowHeights = renderedTableRowHeights r
        colWidths = renderedTableColumnWidths r

        contentWidth = sum colWidths
        contentHeight = sum rowHeights

        hBorderLength = contentWidth + if drawColumnBorders cfg
                                       then max (length colWidths - 1) 0
                                       else 0
        vBorderHeight = contentHeight + if drawRowBorders cfg
                                        then max (length rowHeights - 1) 0
                                        else 0
        horizBorder = hLimit hBorderLength hBorder
        vertBorder = vLimit vBorderHeight vBorder

        leftBorder =
            vBox [topLeftCorner, vertBorder, bottomLeftCorner]
        rightBorder =
            vBox [topRightCorner, vertBorder, bottomRightCorner]

        maybeWrap check f =
            if check cfg then f else id
        addSurroundingBorder b =
            leftBorder <+> (horizBorder <=> b <=> horizBorder) <+> rightBorder
        addRowBorders =
            intersperse horizBorder

        rowsWithColumnBorders = (\(h, row) -> hBox $ maybeColumnBorders h row) <$> zip rowHeights rows
        maybeColumnBorders height = maybeIntersperse cfg drawColumnBorders (vLimit height vBorder)
        body = vBox $
               maybeWrap drawRowBorders addRowBorders rowsWithColumnBorders

    return $ maybeWrap drawSurroundingBorder addSurroundingBorder body

tableCellLayout :: Table n -> RenderM n (RenderedTableCells n)
tableCellLayout t = do
    ctx <- getContext
    cellResults <- forM (tableRows t) $ mapM render

    let rowHeights = rowHeight <$> cellResults
        colWidths = colWidth <$> transpose cellResults
        numRows = length rowHeights
        numCols = if length cellResults >= 1
                     then length (cellResults !! 0)
                     else 0
        allRowAligns = (\i -> M.findWithDefault (defaultRowAlignment t) i (rowAlignments t)) <$>
                       [0..numRows - 1]
        allColAligns = (\i -> M.findWithDefault (defaultColumnAlignment t) i (columnAlignments t)) <$>
                       [0..numCols - 1]
        rowHeight = maximum . fmap (imageHeight . image)
        colWidth = maximum . fmap (imageWidth . image)

        toW = Widget Fixed Fixed . return
        fillEmptyCell w h result =
            if imageWidth (image result) == 0 && imageHeight (image result) == 0
            then result { image = charFill (ctx^.attrL) ' ' w h }
            else result
        mkRow (vAlign, height, rowCells) =
            let paddedCells = flip map (zip3 allColAligns colWidths rowCells) $ \(hAlign, width, cell) ->
                    applyColAlignment width hAlign $
                    applyRowAlignment height vAlign $
                    toW $
                    fillEmptyCell width height cell
            in paddedCells

    let rows = mkRow <$> zip3 allRowAligns rowHeights cellResults

    return $ RenderedTableCells { renderedTableRows = rows
                                , renderedTableColumnWidths = colWidths
                                , renderedTableRowHeights = rowHeights
                                , borderConfiguration = tableBorderConfiguration t
                                }

maybeIntersperse :: BorderConfiguration -> (BorderConfiguration -> Bool) -> Widget n -> [Widget n] -> [Widget n]
maybeIntersperse cfg f v | f cfg = intersperse v
                         | otherwise = id

topLeftCorner :: Widget n
topLeftCorner = joinableBorder $ Edges False True False True

topRightCorner :: Widget n
topRightCorner = joinableBorder $ Edges False True True False

bottomLeftCorner :: Widget n
bottomLeftCorner = joinableBorder $ Edges True False False True

bottomRightCorner :: Widget n
bottomRightCorner = joinableBorder $ Edges True False True False

-- | Given a "table row" of widgets, align each one according to the
-- list of specified column alignments in columns of the specified
-- widths.
alignColumns :: [ColumnAlignment]
             -- ^ The column alignments to use for each widget,
             -- respectively.
             -> [Int]
             -- ^ The width of each column in terminal columns,
             -- respectively.
             -> [Widget n]
             -- ^ The column cells to align.
             -> [Widget n]
alignColumns as widths cells =
    (\(w, a, c) -> applyColAlignment w a c) <$> zip3 widths as cells

applyColAlignment :: Int -> ColumnAlignment -> Widget n -> Widget n
applyColAlignment width align w =
    hLimit width $ case align of
        AlignLeft   -> padRight Max w
        AlignCenter -> hCenter w
        AlignRight  -> padLeft Max w

applyRowAlignment :: Int -> RowAlignment -> Widget n -> Widget n
applyRowAlignment rHeight align w =
    vLimit rHeight $ case align of
        AlignTop    -> padBottom Max w
        AlignMiddle -> vCenter w
        AlignBottom -> padTop Max w
