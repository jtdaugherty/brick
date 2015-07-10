{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Control.Monad (void)
import Data.Monoid
import Graphics.Vty hiding (translate)
import qualified Data.Text as T

import Brick.Main
import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Brick.Widgets.Edit
import Brick.Widgets.List
import Brick.Util
import Brick.AttrMap
import Brick.Markup
import Data.Text.Markup

styles :: [(T.Text, BorderStyle)]
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

editHighlightedKw1Attr :: AttrName
editHighlightedKw1Attr = editAttr <> "kw1"

editHighlightedKw2Attr :: AttrName
editHighlightedKw2Attr = editAttr <> "kw2"

kw :: Widget -> Widget
kw = withAttr keywordAttr

highlightWord :: (Eq a) => String -> a -> Markup a -> Markup a
highlightWord w att mk = assignAttrs 0 chunks mk
    where
        wordLen = length w
        s = toText mk
        chunks = T.splitOn (T.pack w) s

        assignAttrs _ [] m = m
        assignAttrs _ [_] m = m
        assignAttrs pos (t:ts) m = markupSet (pos + T.length t, wordLen) att $ assignAttrs (pos + T.length t + wordLen) ts m

applyMarkup :: String -> Markup AttrName
applyMarkup s =
    highlightWord "foo" editHighlightedKw1Attr $
    highlightWord "bar" editHighlightedKw2Attr $
    (T.pack s) @? editAttr

drawEditString :: String -> Widget
drawEditString = markup . applyMarkup

drawUI :: St -> [Widget]
drawUI st = [withBorderStyle bs a]
    where
        (bsName, bs) = styles !! (st^.stBorderStyle)
        box = borderWithLabel (txt bsName) $
                  (hLimit 25 (
                    (renderEditor $ st^.stEditor)
                    <=> hBorder
                    <=> (vLimit 10 $ renderList (st^.stList))
                  ))
        a = translateBy (st^.stTrans) $ vCenter $
              (hCenter box)
              <=> " "
              <=> (hCenter (kw "Enter" <+> " adds a list item"))
              <=> (hCenter (kw "+" <+> " changes border styles"))
              <=> (hCenter (kw "Arrow keys" <+> " navigates the list"))
              <=> (hCenter (kw "Ctrl-Arrow keys" <+> " move the interface"))

appEvent :: St -> Event -> EventM (Next St)
appEvent st e =
    case e of
        EvKey (KChar '+') [] ->
            continue $ st & stBorderStyle %~ ((`mod` (length styles)) . (+ 1))

        EvKey KEsc [] -> halt st

        EvKey (KChar 'r') [MCtrl] -> suspendAndResume $ do
            putStrLn "Suspended. Press any key..."
            void getChar
            return st

        EvKey KUp [MCtrl] ->    continue $ st & stTrans.row %~ subtract 1
        EvKey KDown [MCtrl] ->  continue $ st & stTrans.row %~ (+ 1)

        EvKey KLeft [MCtrl] ->  continue $ st & stTrans.column %~ (subtract 1)
        EvKey KRight [MCtrl] -> continue $ st & stTrans.column %~ (+ 1)

        EvKey KEnter [] ->
            let el = length $ st^.stList.listElementsL
            in continue $ st & stList %~ (listMoveBy 1 . listInsert el el)

        ev -> continue $ st & stEditor %~ (handleEvent ev)
                            & stList %~ (handleEvent ev)

initialState :: St
initialState =
    St { _stEditor = editor (Name "edit") drawEditString ""
       , _stList = list (Name "list") listDrawElem []
       , _stBorderStyle = 0
       , _stTrans = Location (0, 0)
       }

listDrawElem :: Bool -> Int -> Widget
listDrawElem sel i =
    let selStr s = if sel then "<" <> s <> ">" else s
    in hCenterWith (Just ' ') $ vBox $ (flip map) [1..i+1] $ \j ->
        str $ "Item " <> (selStr $ show i) <> " L" <> show j

theAttrMap :: AttrMap
theAttrMap = attrMap defAttr
    [ (listSelectedAttr,          white `on` blue)
    , (editAttr,                  white `on` blue)
    , (editHighlightedKw1Attr,    fg magenta)
    , (editHighlightedKw2Attr,    fg cyan)
    , (keywordAttr,               fg blue)
    , (borderAttr,                fg blue)
    , (hBorderLabelAttr,          fg cyan)
    ]

theApp :: App St Event
theApp =
    App { appDraw = drawUI
        , appChooseCursor = showFirstCursor
        , appStartEvent = return
        , appHandleEvent = appEvent
        , appAttrMap = const theAttrMap
        , appLiftVtyEvent = id
        }

main :: IO ()
main = do
    st <- defaultMain theApp initialState
    putStrLn $ "You entered: " <> (getEditContents $ st^.stEditor)
