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

data Name = MainUI | Layer | Info deriving (Show, Ord, Eq)

data DragState =
    NotDragging
    | LastLocation T.Location Bool
    deriving (Show)

data St =
    St { _draggableLayerLocation :: T.Location
       , _lastDragLoc :: DragState
       , _clicked :: [T.Extent Name]
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ infoLayer st
    , draggableLayer st
    ]

infoLayer :: St -> Widget Name
infoLayer st = T.Widget T.Fixed T.Fixed $ do
    c <- T.getContext
    let h = c^.T.availHeightL
    T.render $ translateBy (T.Location (0, h-2)) $ reportExtent Info $ dragInfo st

dragInfo :: St -> Widget Name
dragInfo st =
    let infoStr = case st^.lastDragLoc of
          NotDragging -> str "Not dragging"
          LastLocation (T.Location (c,r)) b ->
              str $ "Dragging at column " <> show c <> ", row " <> show r <> " " <>
                    if b
                    then "(dragging layer)"
                    else "(dragging outside of layer)"
    in withDefAttr "info" $ C.hCenter (str $ show $ st^.clicked) <=> C.hCenter infoStr

draggableLayer :: St -> Widget Name
draggableLayer st =
    let highlight = case st^.lastDragLoc of
          LastLocation _ True -> withDefAttr "dragging"
          _ -> id
    in reportExtent MainUI $
       (translateBy (st^.draggableLayerLocation) $
       reportExtent Layer $
       highlight $
       B.border $ str $ "This layer can be dragged by\n" <>
                        "clicking and dragging anywhere\n" <>
                        "on or within its border.") <+> fill ' '

appEvent :: St -> V.Event -> T.EventM Name (T.Next St)
appEvent st (V.EvKey V.KEsc []) = M.halt st
appEvent st ev = do
    Just e <- M.lookupExtent Layer
    M.continue =<< case ev of
          -- If the mouse button was released, stop dragging.
          (V.EvMouseUp _ _ _) ->
              return $ st & lastDragLoc .~ NotDragging
                          & clicked .~ []
          (V.EvMouseDown c r V.BLeft _) -> do
              let mouseLoc = T.Location (c, r)
                  st' = case st^.lastDragLoc of
                          NotDragging
                              -- If the mouse button was down in the layer and
                              -- we were not already dragging it, start dragging
                              -- the layer.
                              | M.clickedExtent (c, r) e -> st & lastDragLoc .~ LastLocation mouseLoc True
                              -- If the mouse button was down outside the layer,
                              -- start dragging outside the layer.
                              | otherwise              -> st & lastDragLoc .~ LastLocation mouseLoc False
                          -- If the mouse button was down and we were already
                          -- dragging, update the drag location. If the drag
                          -- was a continuation of a layer movement, update the
                          -- layer location.
                          LastLocation (T.Location (lc, lr)) bound ->
                              let off = T.Location (c-lc, r-lr)
                              in st & lastDragLoc .~ LastLocation mouseLoc bound
                                    & draggableLayerLocation %~ if bound then (<> off) else id
              es <- M.findClickedExtents (c, r)
              return $ st' & clicked .~ es
          _ -> return st

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ ("info",      V.white `on` V.magenta)
    , ("dragging",  V.black `on` V.yellow)
    ]

app :: M.App St V.Event Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appLiftVtyEvent = id
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (0, 0)) NotDragging []
