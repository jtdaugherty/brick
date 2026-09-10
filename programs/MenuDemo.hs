{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
module Main where

import Lens.Micro ((^.), (.~), (&), Lens')
import Lens.Micro.TH (makeLenses)
import Lens.Micro.Mtl
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid ((<>))
#endif
import qualified Graphics.Vty as V
import qualified Data.Vector as Vec

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
    St { _fileMenuState :: Menu St Name (T.EventM Name St ())
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
       then handleMenuEvent fileMenuState e
       else handleNonMenuEvent e

selectNextEntry :: Menu s n k -> Menu s n k
selectNextEntry m =
    case matching Vec.!? 0 of
        Nothing -> m
        Just (newIdx, _) -> m & menuSelectedIndexL .~ Just newIdx
    where
        dropAmt = case m^.menuSelectedIndexL of
                 Nothing -> 0
                 Just i -> i + 1
        is = m^.menuItemsL
        matching = Vec.filter (isEntry . snd) items
        pairs = Vec.zip (Vec.enumFromN 0 (Vec.length is)) is
        items = Vec.drop dropAmt $ pairs <> pairs
        isEntry (MIEntry {}) = True
        isEntry _ = False

selectPrevEntry :: Menu s n k -> Menu s n k
selectPrevEntry m =
    case matching Vec.!? 0 of
        Nothing -> m
        Just (newIdx, _) -> m & menuSelectedIndexL .~ Just newIdx
    where
        takeAmt = case m^.menuSelectedIndexL of
                 Nothing -> 0
                 Just i -> i
        is = m^.menuItemsL
        matching = Vec.filter (isEntry . snd) items
        pairs = Vec.zip (Vec.enumFromN 0 (Vec.length is)) is
        items = Vec.reverse $ pairs <> Vec.take takeAmt pairs
        isEntry (MIEntry {}) = True
        isEntry _ = False

-- handleMenuEvent :: Lens' St (Menu St Name (T.EventM Name St ())) -> T.BrickEvent Name e -> T.EventM Name St ()
handleMenuEvent :: (Eq n) => Lens' s (Menu s n k) -> T.BrickEvent n e -> T.EventM n s ()
handleMenuEvent which (T.VtyEvent (V.EvKey V.KEnter [])) = do
    sel <- use (which.menuSelectedIndexL)
    handler <- use (which.menuEventHandlerL)
    is <- use (which.menuItemsL)
    case sel of
        Nothing -> return ()
        Just idx ->
            case is Vec.!? idx of
                Just (MIEntry entry) -> do
                    which.menuIsOpenL %= not
                    handler $ menuEntryEvent entry
                _ -> return ()
handleMenuEvent which (T.VtyEvent (V.EvKey V.KDown [])) =
    which %= selectNextEntry
handleMenuEvent which (T.VtyEvent (V.EvKey V.KUp [])) = do
    which %= selectPrevEntry
handleMenuEvent which (T.MouseDown n _ _ (T.Location (_, row))) = do
    mkRegionName <- use (which.menuRegionNameBuilderL)
    if | mkRegionName MenuTitleRegion == n ->
           which.menuIsOpenL %= not
       | mkRegionName MenuBodyRegion  == n -> do
           -- Map the location to the clicked menu entry
           is <- use (which.menuItemsL)
           handler <- use (which.menuEventHandlerL)
           case is Vec.!? row of
               Just (MIEntry entry) -> do
                   which.menuIsOpenL %= not
                   handler $ menuEntryEvent entry
               _ -> return ()
       | otherwise -> return ()
handleMenuEvent which (T.VtyEvent (V.EvMouseDown {})) =
    which.menuIsOpenL %= not
handleMenuEvent which (T.VtyEvent (V.EvKey V.KEsc [])) =
    -- Esc closes the menu
    which.menuIsOpenL %= not
handleMenuEvent _ _ =
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

fileMenu :: Menu St Name (T.EventM Name St ())
fileMenu =
    menu "File" FileMenu
        [ menuEntry "New..." (const True) (return ())
        , menuEntry "Open..." (const True) (return ())
        , menuSeparator
        , menuGap
        , menuEntry "Exit" (const True) M.halt
        ]
        id

main :: IO ()
main = do
    void $ M.defaultMain app $ St fileMenu Nothing
