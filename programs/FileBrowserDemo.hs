{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Main where

import qualified Control.Exception as E
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import qualified Graphics.Vty as V

import Control.Monad.State (get)
import qualified Data.Text as Text
import qualified Brick.Main as M
import qualified Brick.Widgets.List as L
import Brick.AttrMap (AttrName, attrName)
import Brick.Types
  ( Widget
  , BrickEvent(..)
  )
import Brick.Widgets.Center
  ( center
  , hCenter
  )
import Brick.Widgets.Border
  ( borderWithLabel
  )
import Brick.Widgets.Core
  ( vBox, (<=>), padTop
  , hLimit, vLimit, txt
  , withDefAttr, emptyWidget
  , Padding(..)
  )
import qualified Brick.Widgets.FileBrowser as FB
import qualified Brick.AttrMap as A
import Brick.Util (on, fg)
import qualified Brick.Types as T

data Name = FileBrowser1
          deriving (Eq, Show, Ord)

drawUI :: FB.FileBrowser Name -> [Widget Name]
drawUI b = [center $ ui <=> help]
    where
        ui = hCenter $
             vLimit 15 $
             hLimit 50 $
             borderWithLabel (txt "Choose a file") $
             FB.renderFileBrowser True b
        help = padTop (Pad 1) $
               vBox [ case FB.fileBrowserException b of
                          Nothing -> emptyWidget
                          Just e -> hCenter $ withDefAttr errorAttr $
                                    txt $ Text.pack $ E.displayException e
                    , hCenter $ txt "Up/Down: select"
                    , hCenter $ txt "/: search, Ctrl-C or Esc: cancel search"
                    , hCenter $ txt "Enter: change directory or select file"
                    , hCenter $ txt "Esc: quit"
                    ]

appEvent :: BrickEvent Name e -> T.EventM Name (FB.FileBrowser Name) ()
appEvent (VtyEvent ev) = do
    b <- get
    case ev of
        V.EvKey V.KEsc [] | not (FB.fileBrowserIsSearching b) ->
            M.halt
        _ -> do
            FB.handleFileBrowserEvent ev
            -- If the browser has a selected file after handling the
            -- event (because the user pressed Enter), shut down.
            case ev of
                V.EvKey V.KEnter [] -> do
                    b' <- get
                    case FB.fileBrowserSelection b' of
                        [] -> return ()
                        _ -> M.halt
                _ -> return ()
appEvent _ = return ()

errorAttr :: AttrName
errorAttr = attrName "error"

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr
    [ (L.listSelectedFocusedAttr, V.black `on` V.yellow)
    , (FB.fileBrowserCurrentDirectoryAttr, V.white `on` V.blue)
    , (FB.fileBrowserSelectionInfoAttr, V.white `on` V.blue)
    , (FB.fileBrowserDirectoryAttr, fg V.blue)
    , (FB.fileBrowserBlockDeviceAttr, fg V.magenta)
    , (FB.fileBrowserCharacterDeviceAttr, fg V.green)
    , (FB.fileBrowserNamedPipeAttr, fg V.yellow)
    , (FB.fileBrowserSymbolicLinkAttr, fg V.cyan)
    , (FB.fileBrowserUnixSocketAttr, fg V.red)
    , (FB.fileBrowserSelectedAttr, V.white `on` V.magenta)
    , (errorAttr, fg V.red)
    ]

theApp :: M.App (FB.FileBrowser Name) e Name
theApp =
    M.App { M.appDraw = drawUI
          , M.appChooseCursor = M.showFirstCursor
          , M.appHandleEvent = appEvent
          , M.appStartEvent = return ()
          , M.appAttrMap = const theMap
          }

main :: IO ()
main = do
    b <- M.defaultMain theApp =<< FB.newFileBrowser FB.selectNonDirectories FileBrowser1 Nothing
    putStrLn $ "Selected entry: " <> show (FB.fileBrowserSelection b)
