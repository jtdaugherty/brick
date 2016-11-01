{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Lens.Micro ((^.), (&), (%~))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void)
import Data.Default
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.Types (rowL, columnL, Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( translateBy
  , str
  )

data St =
    St { _topLayerLocation :: T.Location
       , _bottomLayerLocation :: T.Location
       }

makeLenses ''St

drawUi :: St -> [Widget ()]
drawUi st =
    [ C.centerLayer $
      B.border $ str "This layer is centered but other\nlayers are placed underneath it."
    , topLayer st
    , bottomLayer st
    ]

topLayer :: St -> Widget ()
topLayer st =
    translateBy (st^.topLayerLocation) $
    B.border $ str "Top layer\n(Arrow keys move)"

bottomLayer :: St -> Widget ()
bottomLayer st =
    translateBy (st^.bottomLayerLocation) $
    B.border $ str "Bottom layer\n(Ctrl-arrow keys move)"

appEvent :: St -> T.BrickEvent () e -> T.EventM () (T.Next St)
appEvent st (T.VtyEvent (V.EvKey V.KDown []))  = M.continue $ st & topLayerLocation.rowL %~ (+ 1)
appEvent st (T.VtyEvent (V.EvKey V.KUp []))    = M.continue $ st & topLayerLocation.rowL %~ (subtract 1)
appEvent st (T.VtyEvent (V.EvKey V.KRight [])) = M.continue $ st & topLayerLocation.columnL %~ (+ 1)
appEvent st (T.VtyEvent (V.EvKey V.KLeft []))  = M.continue $ st & topLayerLocation.columnL %~ (subtract 1)

appEvent st (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) = M.continue $ st & bottomLayerLocation.rowL %~ (+ 1)
appEvent st (T.VtyEvent (V.EvKey V.KUp    [V.MCtrl])) = M.continue $ st & bottomLayerLocation.rowL %~ (subtract 1)
appEvent st (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) = M.continue $ st & bottomLayerLocation.columnL %~ (+ 1)
appEvent st (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) = M.continue $ st & bottomLayerLocation.columnL %~ (subtract 1)

appEvent st (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt st
appEvent st _ = M.continue st

app :: M.App St e ()
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const def
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (0, 0)) (T.Location (0, 0))
