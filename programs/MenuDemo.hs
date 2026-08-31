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
    St { _fileMenuState :: Menu St Name
       , _lastClicked :: Maybe Int
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ renderMenu st (st^.fileMenuState)
    , padTop Max $
      hCenter $
      str $
      "Last clicked menu item: " <> show (st^.lastClicked)
    ]

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.MouseDown (FileMenu MenuTitleRegion) _ _ _) =
    fileMenuState.menuIsOpenL %= not
appEvent e = do
    isOpen <- use (fileMenuState.menuIsOpenL)
    if isOpen
       then handleMenuEvent e
       else handleNonMenuEvent e

handleMenuEvent :: T.BrickEvent Name e -> T.EventM Name St ()
handleMenuEvent (T.MouseDown (FileMenu (MenuEntryRegion i)) _ _ _) =
    lastClicked .= Just i
handleMenuEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    -- Esc closes the menu
    fileMenuState.menuIsOpenL %= not
handleMenuEvent _ =
    return ()

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

fileMenu :: Menu St Name
fileMenu =
    menu "File" FileMenu
        [ menuEntry "Open..." (const True) (return ())
        , menuSeparator
        , menuEntry "Exit" (const True) M.halt
        ]

main :: IO ()
main = do
    void $ M.defaultMain app $ St fileMenu Nothing
