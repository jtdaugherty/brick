{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Default
import Data.Monoid
import Graphics.Vty hiding (translate)
import System.Exit

import Brick.Main
import Brick.Edit
import Brick.List
import Brick.Core
import Brick.Render
import Brick.Center
import Brick.Border
import Brick.Border.Style
import Brick.Util
import Brick.AttrMap

styles :: [(String, BorderStyle)]
styles =
    [ ("ascii", ascii)
    , ("uni", unicode)
    , ("uni-bold", unicodeBold)
    , ("uni-rounded", unicodeRounded)
    ]

data St =
    St { _stEditor :: Editor
       , _stList :: List Int
       , _stBorderStyle :: Int
       , _stTrans :: Location
       }

makeLenses ''St

keywordAttr :: AttrName
keywordAttr = "app" <> "keyword"

kw :: Render -> Render
kw = withAttrName keywordAttr

drawUI :: St -> [Render]
drawUI st = [withBorderStyle bs a]
    where
        (bsName, bs) = styles !! (st^.stBorderStyle)
        box = borderWithLabel bsName $
                  (hLimit 25 (
                    (renderEditor (st^.stEditor))
                    <=> hBorder
                    <=> (vLimit 10 $ renderList (st^.stList))
                  ))
        a = translateBy (st^.stTrans) $ vCenter $
              (hCenter box)
              <=> (vLimit 1 $ vPad ' ')
              <=> (hCenter (kw "Enter" <+> " adds a list item"))
              <=> (hCenter (kw "+" <+> " changes border styles"))
              <=> (hCenter (kw "Arrow keys" <+> " navigates the list"))
              <=> (hCenter (kw "Ctrl-Arrow keys" <+> " move the interface"))

appEvent :: Event -> St -> IO St
appEvent e st =
    case e of
        EvKey (KChar '+') [] ->
            return $ st & stBorderStyle %~ ((`mod` (length styles)) . (+ 1))

        EvKey KEsc [] -> exitSuccess

        EvKey KUp [MCtrl] -> return $ st & stTrans %~ (\(Location (w, h)) -> Location (w, h - 1))
        EvKey KDown [MCtrl] -> return $ st & stTrans %~ (\(Location (w, h)) -> Location (w, h + 1))
        EvKey KLeft [MCtrl] -> return $ st & stTrans %~ (\(Location (w, h)) -> Location (w - 1, h))
        EvKey KRight [MCtrl] -> return $ st & stTrans %~ (\(Location (w, h)) -> Location (w + 1, h))

        EvKey KEnter [] ->
            let el = length $ listElements $ st^.stList
            in return $ st & stList %~ (listMoveBy 1 . listInsert el el)

        ev -> return $ st & stEditor %~ (handleEvent ev)
                          & stList %~ (handleEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (Name "edit") ""
       , _stList = list (Name "list") listDrawElem []
       , _stBorderStyle = 0
       , _stTrans = Location (0, 0)
       }

listDrawElem :: Bool -> Int -> Render
listDrawElem sel i =
    let selStr s = if sel then "<" <> s <> ">" else s
    in hCenterWith (Just ' ') $ vBox $ for [1..i+1] $ \j ->
        (txt $ "Item " <> (selStr $ show i) <> " L" <> show j, High)

theAttrMap :: AttrMap
theAttrMap = attrMap defAttr
    [ (listSelectedAttr,   white `on` blue)
    , (editAttr,           green `on` white)
    , (keywordAttr,        fg blue)
    , (borderAttr,         fg blue)
    , (hBorderLabelAttr,   fg cyan)
    ]

theApp :: App St Event
theApp =
    def { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        , appAttrMap = const theAttrMap
        }

main :: IO ()
main = defaultMain theApp initialState
