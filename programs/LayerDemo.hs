{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.Types (locationRowL, locationColumnL, Location(..), Widget)
import qualified Brick.Main as M
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import Brick.Widgets.Core
  ( translateBy
  , str
  , relativeTo
  , reportExtent
  , withDefAttr
  )
import Brick.Util (fg)
import Brick.AttrMap
  ( attrMap
  , AttrName
  , attrName
  )

data St =
    St { _middleLayerLocation :: T.Location
       , _bottomLayerLocation :: T.Location
       }

makeLenses ''St

data Name =
    MiddleLayerElement
    deriving (Ord, Eq, Show)

drawUi :: St -> [Widget Name]
drawUi st =
    [ C.centerLayer $
      B.border $ str "This layer is centered but other\nlayers are placed underneath it."
    , arrowLayer
    , middleLayer st
    , bottomLayer st
    ]

arrowLayer :: Widget Name
arrowLayer =
    let msg = "Relatively\n" <>
              "positioned\n" <>
              "arrow---->"
    in relativeTo MiddleLayerElement (Location (-10, -2)) $
       withDefAttr arrowAttr $
       str msg

middleLayer :: St -> Widget Name
middleLayer st =
    translateBy (st^.middleLayerLocation) $
    reportExtent MiddleLayerElement $
    B.border $ str "Middle layer\n(Arrow keys move)"

bottomLayer :: St -> Widget Name
bottomLayer st =
    translateBy (st^.bottomLayerLocation) $
    B.border $ str "Bottom layer\n(Ctrl-arrow keys move)"

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey V.KDown []))  =
    middleLayerLocation.locationRowL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KUp []))    =
    middleLayerLocation.locationRowL %= (subtract 1)
appEvent (T.VtyEvent (V.EvKey V.KRight [])) =
    middleLayerLocation.locationColumnL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KLeft []))  =
    middleLayerLocation.locationColumnL %= (subtract 1)

appEvent (T.VtyEvent (V.EvKey V.KDown  [V.MCtrl])) =
    bottomLayerLocation.locationRowL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KUp    [V.MCtrl])) =
    bottomLayerLocation.locationRowL %= (subtract 1)
appEvent (T.VtyEvent (V.EvKey V.KRight [V.MCtrl])) =
    bottomLayerLocation.locationColumnL %= (+ 1)
appEvent (T.VtyEvent (V.EvKey V.KLeft  [V.MCtrl])) =
    bottomLayerLocation.locationColumnL %= (subtract 1)

appEvent (T.VtyEvent (V.EvKey V.KEsc [])) = M.halt
appEvent _ = return ()

arrowAttr :: AttrName
arrowAttr = attrName "attr"

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = return ()
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const $ attrMap V.defAttr [(arrowAttr, fg V.cyan)]
          , M.appChooseCursor = M.neverShowCursor
          }

main :: IO ()
main = void $ M.defaultMain app $ St (T.Location (20, 5)) (T.Location (0, 0))
