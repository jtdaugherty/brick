{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (^.), (&), (%~))
import Control.Monad (void)
import Data.Default
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.Types (row, column)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import Brick.Widgets.Core
  ( Widget
  , translateBy
  )

data St =
    St { _topLayerLocation :: T.Location
       , _bottomLayerLocation :: T.Location
       }

makeLenses ''St

drawUi :: St -> [Widget]
drawUi st =
    [ topLayer st
    , bottomLayer st
    ]

topLayer :: St -> Widget
topLayer st =
    translateBy (st^.topLayerLocation) $
    B.border "Top layer\n(Arrow keys move)"

bottomLayer :: St -> Widget
bottomLayer st =
    translateBy (st^.bottomLayerLocation) $
    B.border "Bottom layer\n(Ctrl-arrow keys move)"

appEvent :: St -> V.Event -> M.EventM (M.Next St)
appEvent st (V.EvKey V.KDown [])  = M.continue $ st & topLayerLocation.row %~ (+ 1)
appEvent st (V.EvKey V.KUp [])    = M.continue $ st & topLayerLocation.row %~ (subtract 1)
appEvent st (V.EvKey V.KRight []) = M.continue $ st & topLayerLocation.column %~ (+ 1)
appEvent st (V.EvKey V.KLeft [])  = M.continue $ st & topLayerLocation.column %~ (subtract 1)

appEvent st (V.EvKey V.KDown  [V.MCtrl]) = M.continue $ st & bottomLayerLocation.row %~ (+ 1)
appEvent st (V.EvKey V.KUp    [V.MCtrl]) = M.continue $ st & bottomLayerLocation.row %~ (subtract 1)
appEvent st (V.EvKey V.KRight [V.MCtrl]) = M.continue $ st & bottomLayerLocation.column %~ (+ 1)
appEvent st (V.EvKey V.KLeft  [V.MCtrl]) = M.continue $ st & bottomLayerLocation.column %~ (subtract 1)

appEvent st (V.EvKey V.KEsc []) = M.halt st
appEvent st _ = M.continue st

app :: M.App St V.Event
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const def
          , M.appMakeVtyEvent = id
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (0, 0)) (T.Location (0, 0))
