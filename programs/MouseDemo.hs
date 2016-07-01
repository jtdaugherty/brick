{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.), (&), (%~), (.~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Default
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.Types (rowL, columnL, Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core

data DragState =
    NotDragging
    | LastLocation T.Location Bool
    deriving (Show)

data St =
    St { _topLayerLocation :: T.Location
       , _bottomLayerLocation :: T.Location
       , _extent :: Maybe (T.Extent ())
       , _lastDragLoc :: DragState
       }

makeLenses ''St

drawUi :: St -> [Widget ()]
drawUi st =
    [ C.centerLayer $
      B.border $ str $ "This layer is centered but other\nlayers are visible underneath it.\n" <>
                     (show $ st^.extent) <> " " <> (show $ st^.lastDragLoc)
    , topLayer st
    , bottomLayer st
    ]

topLayer :: St -> Widget ()
topLayer st =
    translateBy (st^.topLayerLocation) $
    reportExtent () $
    B.border $ str "Top layer\n(Arrow keys move)"

bottomLayer :: St -> Widget ()
bottomLayer st =
    translateBy (st^.bottomLayerLocation) $
    B.border $ str "Bottom layer\n(Ctrl-arrow keys move)"

appEvent :: St -> V.Event -> T.EventM () (T.Next St)
appEvent st (V.EvKey V.KEsc []) = M.halt st
appEvent st ev = do
    Just e <- M.lookupExtent ()
    let s = case ev of
          (V.EvMouseUp _ _ _) ->
              st & lastDragLoc .~ NotDragging
          (V.EvMouseDown c r V.BLeft _) ->
              let mouseLoc = T.Location (c-1, r-1)
              in case st^.lastDragLoc of
                  NotDragging
                      | clickedExtent (c, r) e -> st & lastDragLoc .~ LastLocation mouseLoc True
                      | otherwise              -> st & lastDragLoc .~ LastLocation mouseLoc False
                  LastLocation (T.Location (lc, lr)) bound ->
                      let off = T.Location (c-1-lc, r-1-lr)
                      in st & lastDragLoc .~ LastLocation mouseLoc bound
                            & topLayerLocation %~ if bound then (<> off) else id

          (V.EvKey V.KDown [])           -> st & topLayerLocation.rowL %~ (+ 1)
          (V.EvKey V.KUp [])             -> st & topLayerLocation.rowL %~ (subtract 1)
          (V.EvKey V.KRight [])          -> st & topLayerLocation.columnL %~ (+ 1)
          (V.EvKey V.KLeft [])           -> st & topLayerLocation.columnL %~ (subtract 1)
          (V.EvKey V.KDown  [V.MCtrl])   -> st & bottomLayerLocation.rowL %~ (+ 1)
          (V.EvKey V.KUp    [V.MCtrl])   -> st & bottomLayerLocation.rowL %~ (subtract 1)
          (V.EvKey V.KRight [V.MCtrl])   -> st & bottomLayerLocation.columnL %~ (+ 1)
          (V.EvKey V.KLeft  [V.MCtrl])   -> st & bottomLayerLocation.columnL %~ (subtract 1)
          _ -> st
    M.continue $ s & extent .~ (Just e)

clickedExtent :: (Int, Int) -> T.Extent n -> Bool
clickedExtent (c', r') (T.Extent _ (T.Location (lc, lr)) (w, h)) =
    let c = c' - 1
        r = r' - 1
    in c >= lc && c < (lc + w) &&
       r >= lr && r < (lr + h)

app :: M.App St V.Event ()
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const def
          , M.appLiftVtyEvent = id
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (0, 0)) (T.Location (0, 0)) Nothing NotDragging
