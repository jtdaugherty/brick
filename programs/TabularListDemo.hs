{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.Mtl
import Lens.Micro.TH
import Control.Monad (void)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import qualified Brick.Main as M
import qualified Brick.Types as T
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Table as Table
import qualified Brick.AttrMap as A
import qualified Data.Vector as Vec
import Brick.Types
  ( Widget
  )
import Brick.Widgets.Core
  ( (<=>)
  , str
  , vLimit
  , hLimit
  , vBox
  , hBox
  , withDefAttr
  )
import Brick.Util (on)

data Row = Row String String String

data AppState =
    AppState { _tabularList :: L.List () Row
             , _colIndex :: Int
             }

makeLenses ''AppState

drawUI :: AppState -> [Widget ()]
drawUI s = [ui]
    where
        l = s^.tabularList
        label = str $ "Row " <> cur <> " / col " <> show (s^.colIndex + 1)
        cur = case l^.(L.listSelectedL) of
                Nothing -> "-"
                Just i -> show (i + 1)
        box = B.borderWithLabel label $
              hLimit totalWidth $
              vLimit 15 $
              listDrawElement 0 False headerRow <=>
              L.renderList (listDrawElement (s^.colIndex)) True l
        ui = C.vCenter $ vBox [ C.hCenter box
                              , str " "
                              , C.hCenter $ str "Press +/- to add/remove list elements."
                              , C.hCenter $ str "Use arrow keys to change selection."
                              , C.hCenter $ str "Press Esc to exit."
                              ]

appEvent :: T.BrickEvent () e -> T.EventM () AppState ()
appEvent (T.VtyEvent e) =
    case e of
        V.EvKey (V.KChar '+') [] -> do
            els <- use (tabularList.L.listElementsL)
            let el = Row (show pos) (show $ pos * 3) (show $ pos * 9)
                pos = Vec.length els
            tabularList %= L.listInsert pos el

        V.EvKey (V.KChar '-') [] -> do
            sel <- use (tabularList.L.listSelectedL)
            case sel of
                Nothing -> return ()
                Just i -> tabularList %= L.listRemove i

        V.EvKey V.KLeft [] ->
            colIndex %= (\i -> max 0 (i - 1))
        V.EvKey V.KRight [] ->
            colIndex %= (\i -> min (length columnAlignments - 1) (i + 1))

        V.EvKey V.KEsc [] -> M.halt

        ev -> T.zoom tabularList $ L.handleListEvent ev
appEvent _ = return ()

listDrawElement :: Int -> Bool -> Row -> Widget ()
listDrawElement colIdx sel (Row a b c) =
    let ws = [str a, str b, str c]
        maybeSelect es = selectCell <$> zip [0..] es
        selectCell (i, w) = if sel && i == colIdx
                            then withDefAttr selectedCellAttr w
                            else w
    in hLimit totalWidth $
       hBox $
       maybeSelect $
       Table.alignColumns columnAlignments columnWidths ws

initialState :: AppState
initialState =
    AppState { _tabularList = L.list () (Vec.fromList initialRows) 1
             , _colIndex = 0
             }

selectedCellAttr :: A.AttrName
selectedCellAttr = A.attrName "selectedCell"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listAttr,            V.white `on` V.blue)
    , (selectedCellAttr,      V.blue `on` V.white)
    ]

columnWidths :: [Int]
columnWidths = [10, 15, 20]

totalWidth :: Int
totalWidth = sum columnWidths

headerRow :: Row
headerRow = Row "Col 1" "Col 2" "Col 3"

columnAlignments :: [Table.ColumnAlignment]
columnAlignments = [Table.AlignLeft, Table.AlignCenter, Table.AlignRight]

initialRows :: [Row]
initialRows =
    [ Row "one" "two" "three"
    , Row "foo" "bar" "baz"
    , Row "stuff" "things" "blah"
    ]

theApp :: M.App AppState e ()
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = void $ M.defaultMain theApp initialState
