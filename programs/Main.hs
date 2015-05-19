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
       }

makeLenses ''St

kw :: Attr
kw = fg blue

drawUI :: St -> [Prim St]
drawUI st = [a]
    where
        (bsName, bs) = styles !! (st^.stBorderStyle)
        a = centered $
              (borderedWithLabel bs bsName $
                (HLimit 25 (
                  (VLimit 1 $ UseAttr (cyan `on` blue) $ With stEditor drawEditor)
                  <<=
                  hBorder bs
                  =>>
                  (VLimit 10 $ With stList drawList)
                )))
              <<=
              (((UseAttr kw "Enter") <+> " adds a list item")
              <=>
              ((UseAttr kw "+") <+> " changes border styles")
              <=>
              ((UseAttr kw "Arrow keys") <+> " navigate the list")
              )

appEvent :: Event -> St -> IO St
appEvent e st =
    case e of
        EvKey (KChar '+') [] ->
            return $ st & stBorderStyle %~ ((`mod` (length styles)) . (+ 1))

        EvKey KEsc [] -> exitSuccess

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
       }

listDrawElem :: Bool -> Int -> Prim (List Int)
listDrawElem sel i =
    let selAttr = white `on` blue
        maybeSelect = if sel
                      then UseAttr selAttr
                      else id
    in maybeSelect $ hCentered (Txt $ "Number " <> show i)

theApp :: App St Event
theApp =
    def { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appHandleEvent = appEvent
        }

main :: IO ()
main = defaultMain theApp initialState
