{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Control.Monad (void, when)
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
import Brick.Widgets.Core (txtWrap, hLimit)
import Brick.Widgets.Center (center)
import Brick.Widgets.Menu
import Brick.Widgets.MenuBar

data Name = FileMenu MenuRegion
          | EditMenu MenuRegion
          | HelpMenu MenuRegion
          deriving (Show, Ord, Eq)

data St =
    St { _menuBar :: SimpleMenuBar St Name
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ renderMenuBar st (st^.menuBar)
    , center $
      hLimit 40 $
      txtWrap $
      Text.unlines $
      [ "Click the menu title with the mouse or press Alt-F to open the menu."
      , ""
      , "When a menu is open:"
      , ""
      , "- Press up/down arrow keys to select items and then " <>
        "press Enter to activate them, or click them with the mouse instead."
      , ""
      , "- Press left/right arrow keys cycle through open menus."
      , ""
      , "Press Esc to quit the program."
      ]
    ]

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent e = do
    handled <- handleMenuBarEvent menuBar e
    when (not handled) $ handleNonMenuBarEvent e

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

newFileMenu :: SimpleMenu St Name
newFileMenu =
    simpleMenu "File" FileMenu
        [ menuEntry "New..." (return ())
        , menuEntry "Open..." (return ())
        , menuSeparator
        , menuEntry "Exit" M.halt
        ]

newEditMenu :: SimpleMenu St Name
newEditMenu =
    simpleMenu "Edit" EditMenu
        [ menuEntry "Undo" (return ())
        , menuEntry "Redo" (return ())
        , menuSeparator
        , menuEntry "Cut" (return ())
        , menuEntry "Copy" (return ())
        , menuEntry "Paste" (return ())
        ]

newHelpMenu :: SimpleMenu St Name
newHelpMenu =
    simpleMenu "Help" HelpMenu
        [ menuEntry "About" (return ())
        , menuEntry "Check for updates" (return ())
        ]

main :: IO ()
main = do
    void $ M.defaultMain app $ St $ newMenuBar [newFileMenu, newEditMenu, newHelpMenu]
