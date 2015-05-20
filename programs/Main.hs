{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Default
import Data.Monoid
import Graphics.Vty
import System.Exit

import Brick.Main
import Brick.Edit
import Brick.List
import Brick.Core
import Brick.Prim
import Brick.Center
import Brick.Border
import Brick.Util

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

kw :: String -> Prim a
kw = UseAttr (fg blue) . Txt

drawUI :: St -> [Prim St]
drawUI st = [a]
    where
        (bsName, bs) = styles !! (st^.stBorderStyle)
        a = Translate (st^.stTrans) $
            vCenter $
              (hCenter $ borderWithLabel bs bsName $
                  (HLimit 25 (
                    (VLimit 1 $ UseAttr (cyan `on` blue) $ drawEditor stEditor)
                    <<=
                    hBorder bs
                    =>>
                    (VLimit 10 $ drawList stList)
                  )))
              <<=
              (VLimit 1 $ VPad ' ')
              =>>
              (hCenter (kw "Enter" <+> " adds a list item"))
              <=>
              (hCenter (kw "+" <+> " changes border styles"))
              <=>
              (hCenter (kw "Arrow keys" <+> " navigates the list"))
              <=>
              (hCenter (kw "Ctrl-Arrow keys" <+> " move the interface"))

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
            in return $ st & stList %~ listInsert el el

        ev -> return $ st & stEditor %~ (handleEvent ev)
                          & stList %~ (handleEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (CursorName "edit") ""
       , _stList = list listDrawElem []
       , _stBorderStyle = 0
       , _stTrans = Location (0, 0)
       }

listDrawElem :: Bool -> Int -> Prim (List Int)
listDrawElem sel i =
    let selAttr = white `on` blue
        maybeSelect = if sel
                      then UseAttr selAttr
                      else id
    in maybeSelect $ hCenter $ VBox $ for [1..i+1] $ \j ->
        (Txt $ "Item " <> show i <> " L" <> show j, High)

theApp :: App St Event
theApp =
    def { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        }

main :: IO ()
main = defaultMain theApp initialState
