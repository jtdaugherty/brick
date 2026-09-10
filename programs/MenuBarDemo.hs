{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lens.Micro ((^.), (&), (.~))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Data.Text as Text
import qualified Graphics.Vty as V

import qualified Brick.Types as T
import Brick.AttrMap
import Brick.Util
import Brick.Types (Widget)
import qualified Brick.Main as M
import Brick.Widgets.Core (txtWrap, hLimit, padLeft, Padding(..))
import Brick.Widgets.Center (center)
import Brick.Widgets.Menu
import Brick.Widgets.MenuBar

data Name = FileMenu MenuRegion
          | EditMenu MenuRegion
          deriving (Show, Ord, Eq)

data St =
    St { _menuBar :: SimpleMenuBar St Name
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ padLeft (Pad 1) $
      renderMenuBar st (st^.menuBar)
    , center $
      hLimit 40 $
      txtWrap $
      Text.unlines $
      [ "Click the menu title with the mouse or press Alt-F to open the menu."
      , ""
      , "When the menu is open, press arrow keys to select items and then " <>
        "press Enter to activate them, or click them with the mouse instead."
      , ""
      , "Press Esc to quit the program."
      ]
    ]

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
-- appEvent (T.MouseDown (FileMenu MenuTitle) _ _ _) =
--     fileMenu.menuIsOpenL %= not
appEvent e = do
    mb <- use menuBar
    let isOpen = hasOpenMenu mb
        isTitleClick = isMenuTitleEvent mb e
    if isOpen || isTitleClick
       then handleMenuBarEvent menuBar e
       else handleNonMenuBarEvent e

handleNonMenuBarEvent :: T.BrickEvent Name e -> T.EventM Name St ()
handleNonMenuBarEvent (T.VtyEvent (V.EvKey V.KEsc [])) =
    -- Esc quits the application
    M.halt
handleNonMenuBarEvent _ =
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

editMenuState :: SimpleMenu St Name
editMenuState =
    simpleMenu "Edit" EditMenu
        [ menuEntry "Undo" (const True) (return ())
        , menuEntry "Redo" (const True) (return ())
        , menuSeparator
        , menuEntry "Cut" (const True) (return ())
        , menuEntry "Copy" (const True) (return ())
        , menuEntry "Paste" (const True) (return ())
        ]

main :: IO ()
main = do
    void $ M.defaultMain app $ St $ newMenuBar [fileMenuState, editMenuState]
