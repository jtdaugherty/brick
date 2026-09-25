{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lens.Micro ((^.))
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
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
import Brick.Widgets.Core (txtWrap, hLimit, padLeft, Padding(..), withBorderStyle)
import Brick.Widgets.Center (center)
import qualified Brick.Widgets.Border.Style as S
import Brick.Widgets.Menu

data Name = FileMenu MenuRegion
          | ExportMenu MenuRegion
          deriving (Show, Ord, Eq)

data St =
    St { _fileMenu :: SimpleMenu St Name
       , _borderStyle :: S.BorderStyle
       }

makeLenses ''St

drawUi :: St -> [Widget Name]
drawUi st =
    [ padLeft (Pad 1) $
      withBorderStyle (st^.borderStyle) $
      renderMenu st (st^.fileMenu)
    , center $
      hLimit 60 $
      txtWrap $
      Text.unlines $
      [ "Click the menu title with the mouse or press Alt-F to open the menu."
      , ""
      , "When the menu is open, press arrow keys to select items and then " <>
        "press Enter to activate them, or click them with the mouse instead."
      , ""
      , "When the menu is open, press Enter or the right arrow key to open " <>
        "the submenu; press Esc or the left arrow key to close it."
      , ""
      , "Press these keys to switch menu border styles:"
      , ""
      ] <>
      [ "- " <> Text.singleton c <> ": " <> label | (c, (label, _)) <- borderStyles] <>
      [ ""
      , "Press Esc to quit the program."
      ]
    ]

borderStyles :: [(Char, (Text.Text, S.BorderStyle))]
borderStyles =
    [ ('1', ("Unicode (default)", S.unicode))
    , ('2', ("Unicode rounded", S.unicodeRounded))
    , ('3', ("Unicode bold", S.unicodeBold))
    , ('4', ("ASCII", S.ascii))
    ]

appEvent :: T.BrickEvent Name e -> T.EventM Name St ()
appEvent (T.VtyEvent (V.EvKey (V.KChar 'f') [V.MMeta])) =
    fileMenu %= toggleMenu
appEvent (T.VtyEvent (V.EvKey (V.KChar c) [])) =
    case lookup c borderStyles of
        Nothing -> return ()
        Just (_, s) -> borderStyle .= s
appEvent e = do
    handled <- handleMenuEvent fileMenu e
    when (not handled) $ handleNonMenuEvent e

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
    , (menuTitleKeyHighlightAttr, style V.underline)
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
    setTitleRenderer (titleHightlightKey 'f') $
    simpleMenu "File" FileMenu
        [ menuEntry "New..." (return ())
        , menuEntry "Open..." (return ())
        , menuSeparator
        , submenu $ simpleMenu "Export" ExportMenu
            [ menuEntry "JPEG" (return ())
            , menuEntry "PNG" (return ())
            , menuEntry "GIF" (return ())
            ]
        , menuSeparator
        , menuEntry "Exit" M.halt
        ]

main :: IO ()
main = void $ M.defaultMain app $ St newFileMenu S.unicode
