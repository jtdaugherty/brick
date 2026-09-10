{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget)
import qualified Brick.Main as M
import Brick.Widgets.Core (padTop, str, Padding(Max))
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Menu

data Name = FileMenu MenuRegion
          deriving (Show, Ord, Eq)

data St =
    St { _fileMenu :: SimpleMenu St Name
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ renderMenu st (st^.fileMenu)
    ]

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.MouseDown (FileMenu MenuTitle) _ _ _) =
    fileMenu.menuIsOpenL %= not
appEvent e = do
    isOpen <- use (fileMenu.menuIsOpenL)
    if isOpen
       then handleMenuEvent fileMenu e
       else handleNonMenuEvent e

handleNonMenuEvent :: T.BrickEvent Name e -> T.EventM Name St ()
handleNonMenuEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    -- Esc quits the application
    M.halt
handleNonMenuEvent _ =
    return ()

aMap :: AttrMap
aMap = attrMap V.defAttr
    [ (menuAttr, fg V.white)
    , (menuTitleAttr, fg V.white)
    , (menuTitleSelectedAttr, V.black `on` V.white)
    , (menuEntryDisabledAttr, fg V.red)
    , (menuEntrySelectedAttr, V.black `on` V.yellow)
    , (menuEntrySelectedDisabledAttr, V.black `on` V.red)
    ]

app :: M.App St e Name
app =
    M.App { M.appDraw = drawUi
          , M.appStartEvent = do
              vty <- M.getVtyHandle
              liftIO $ V.setMode (V.outputIface vty) V.Mouse True
          , M.appHandleEvent = appEvent
          , M.appAttrMap = const aMap
          , M.appChooseCursor = M.showFirstCursor
          }

fileMenuState :: SimpleMenu St Name
fileMenuState =
    simpleMenu "File" FileMenu
        [ menuEntry "New..." (const True) (return ())
        , menuEntry "Open..." (const True) (return ())
        , menuSeparator
        , menuEntry "Exit" (const True) M.halt
        ]

main :: IO ()
main = do
    void $ M.defaultMain app $ St fileMenuState
