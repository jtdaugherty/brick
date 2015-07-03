{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad (void)
import Data.Default
import Graphics.Vty

import Brick.Core
import Brick.Main
import Brick.Widgets.Core
import Brick.Widgets.Border

data St =
    St { _topLayerLocation :: Location
       , _bottomLayerLocation :: Location
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
    border "Top layer\n(Arrow keys move)"

bottomLayer :: St -> Widget
bottomLayer st =
    translateBy (st^.bottomLayerLocation) $
    border "Bottom layer\n(Ctrl-arrow keys move)"

appEvent :: St -> Event -> EventM (Next St)
appEvent st (EvKey KDown []) = continue $ st & topLayerLocation.row %~ (+ 1)
appEvent st (EvKey KUp []) = continue $ st & topLayerLocation.row %~ (subtract 1)
appEvent st (EvKey KRight []) = continue $ st & topLayerLocation.column %~ (+ 1)
appEvent st (EvKey KLeft []) = continue $ st & topLayerLocation.column %~ (subtract 1)
appEvent st (EvKey KDown [MCtrl]) = continue $ st & bottomLayerLocation.row %~ (+ 1)
appEvent st (EvKey KUp [MCtrl]) = continue $ st & bottomLayerLocation.row %~ (subtract 1)
appEvent st (EvKey KRight [MCtrl]) = continue $ st & bottomLayerLocation.column %~ (+ 1)
appEvent st (EvKey KLeft [MCtrl]) = continue $ st & bottomLayerLocation.column %~ (subtract 1)
appEvent st (EvKey KEsc []) = halt st
appEvent st _ = continue st

app :: App St Event
app =
    App { appDraw = drawUi
        , appStartEvent = return
        , appHandleEvent = appEvent
        , appAttrMap = const def
        , appMakeVtyEvent = id
        , appChooseCursor = neverShowCursor
        }

main :: IO ()
main = void $ defaultMain app $ St (Location (0, 0)) (Location (0, 0))
