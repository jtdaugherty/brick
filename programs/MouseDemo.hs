{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.), (&), (%~), (.~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Monoid ((<>))
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core

data DragState =
    NotDragging
    | LastLocation T.Location Bool
    deriving (Show)

data St =
    St { _draggableLayerLocation :: T.Location
       , _lastDragLoc :: DragState
       }

makeLenses ''St

drawUi :: St -> [Widget ()]
drawUi st =
    [ draggableLayer st
    , infoLayer st
    ]

infoLayer :: St -> Widget ()
infoLayer st = fill ' ' <=> dragInfo st

dragInfo :: St -> Widget ()
dragInfo st =
    let infoStr = case st^.lastDragLoc of
          NotDragging -> str "Not dragging"
          LastLocation (T.Location (c,r)) b ->
              str $ "Dragging at column " <> show c <> ", row " <> show r <> " " <>
                    if b
                    then "(dragging layer)"
                    else "(dragging outside of layer)"
    in withDefAttr "info" $ C.hCenter infoStr

draggableLayer :: St -> Widget ()
draggableLayer st =
    let highlight = case st^.lastDragLoc of
          LastLocation _ True -> withDefAttr "dragging"
          _ -> id
    in translateBy (st^.draggableLayerLocation) $
       reportExtent () $
       highlight $
       B.border $ str $ "This layer can be dragged by\n" <>
                        "clicking and dragging anywhere\n" <>
                        "on or within its border."

appEvent :: St -> V.Event -> T.EventM () (T.Next St)
appEvent st (V.EvKey V.KEsc []) = M.halt st
appEvent st ev = do
    Just e <- M.lookupExtent ()
    M.continue $ case ev of
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
                            & draggableLayerLocation %~ if bound then (<> off) else id
          _ -> st

clickedExtent :: (Int, Int) -> T.Extent n -> Bool
clickedExtent (c', r') (T.Extent _ (T.Location (lc, lr)) (w, h)) =
    let c = c' - 1
        r = r' - 1
    in c >= lc && c < (lc + w) &&
       r >= lr && r < (lr + h)

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("info",      V.white `on` V.magenta)
    , ("dragging",  V.black `on` V.yellow)
    ]

app :: M.App St V.Event ()
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appLiftVtyEvent = id
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (0, 0)) NotDragging
