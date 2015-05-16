{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brick where

import Control.Applicative
--import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Default
import Data.Maybe
import Data.String
import Data.Monoid
import Graphics.Vty hiding ((<|>))

newtype Location = Location (Int, Int)

origin :: Location
origin = Location (0, 0)

instance Monoid Location where
    mempty = origin
    mappend (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

newtype Name = Name String
               deriving (Eq, Show)

data CursorLocation =
    CursorLocation { cursorLocation :: !Location
                   , cursorLocationName :: !(Maybe Name)
                   }

data RenderOrder = A | B

data Prim = Fixed String
          | HPad Char
          | VPad Char
          | HFill Char
          | VFill Char
          | HBox Prim Prim RenderOrder
          | VBox Prim Prim RenderOrder
          | HLimit Int Prim
          | VLimit Int Prim
          | UseAttr Attr Prim
          | Raw Image
          | Translate Int Int Prim

mkImage :: DisplayRegion -> Attr -> Prim -> Image
mkImage (w, h) a (Fixed s) =
    if w > 0 && h > 0
    then crop w h $ string a s
    else emptyImage
mkImage (w, h) _ (Raw img) =
    if w > 0 && h > 0
    then crop w h img
    else emptyImage
mkImage (w, h) a (HPad c) = charFill a c w (max 1 h)
mkImage (w, h) a (VPad c) = charFill a c (max 1 w) h
mkImage (w, h) a (HFill c) = charFill a c w (min h 1)
mkImage (w, h) a (VFill c) = charFill a c (min w 1) h
mkImage (_, h) a (HLimit w p) = mkImage (w, h) a p
mkImage (w, _) a (VLimit h p) = mkImage (w, h) a p
mkImage (w, h) _ (UseAttr a p) = mkImage (w, h) a p
mkImage (w, h) a (Translate tw th p) =
    crop w h $ translate tw th $ mkImage (w, h) a p
mkImage (w, h) a (HBox p1 p2 order) = horizCat [first, second]
    where
        (first, second) =
            case order of
                A -> let p1Img = mkImage (w, h) a p1
                         p2Img = mkImage (w - imageWidth p1Img, min h (imageHeight p1Img)) a p2
                     in (p1Img, p2Img)
                B -> let p1Img = mkImage (w - imageWidth p2Img, min h (imageHeight p2Img)) a p1
                         p2Img = mkImage (w, h) a p2
                     in (p1Img, p2Img)
mkImage (w, h) a (VBox p1 p2 order) = vertCat [first, second]
    where
        (first, second) =
            case order of
                A -> let p1Img = mkImage (w, h) a p1
                         p2Img = mkImage (min w (imageWidth p1Img), h - imageHeight p1Img) a p2
                     in (p1Img, p2Img)
                B -> let p1Img = mkImage (min w (imageWidth p2Img), h - imageHeight p2Img) a p1
                         p2Img = mkImage (w, h) a p2
                     in (p1Img, p2Img)

(<<+) :: Prim -> Prim -> Prim
(<<+) a b = HBox a b A

(+>>) :: Prim -> Prim -> Prim
(+>>) a b = HBox a b B

(<<=) :: Prim -> Prim -> Prim
(<<=) a b = VBox a b A

(=>>) :: Prim -> Prim -> Prim
(=>>) a b = VBox a b B

bordered :: Prim -> Prim
bordered wrapped = total
    where
        topBottom = "+" <<+ HFill '-' +>> "+"
        middle = VFill '|' +>> wrapped <<+ VFill '|'
        total = topBottom =>> middle <<= topBottom

instance IsString Prim where
    fromString = Fixed

data Editor =
    Editor { editStr :: !String
           , editCursorPos :: !Int
           , editorName :: !Name
           }

data App a e =
    App { appDraw :: a -> [Prim]
        , appChooseCursor :: a -> [CursorLocation] -> Maybe CursorLocation
        , appHandleEvent :: e -> a -> IO a
        , appHandleResize :: Name -> DisplayRegion -> a -> a
        }

instance Default (App a e) where
    def = App { appDraw = const def
              , appChooseCursor = neverShowCursor
              , appHandleEvent = const return
              , appHandleResize = const $ const id
              }

data FocusRing = FocusRingEmpty
               | FocusRingNonempty ![Name] !Int

clOffset :: CursorLocation -> Location -> CursorLocation
clOffset cl loc = cl { cursorLocation = (cursorLocation cl) <> loc }

editEvent :: Event -> Editor -> Editor
editEvent e theEdit = f theEdit
    where
        f = case e of
              EvKey (KChar 'a') [MCtrl] -> gotoBOL
              EvKey (KChar 'e') [MCtrl] -> gotoEOL
              EvKey (KChar 'd') [MCtrl] -> deleteChar
              EvKey (KChar c) [] | c /= '\t' -> insertChar c
              EvKey KDel [] -> deleteChar
              EvKey KLeft [] -> moveLeft
              EvKey KRight [] -> moveRight
              EvKey KBS [] -> deletePreviousChar
              _ -> id

moveLeft :: Editor -> Editor
moveLeft e = e { editCursorPos = max 0 (editCursorPos e - 1)
               }

moveRight :: Editor -> Editor
moveRight e = e { editCursorPos = min (editCursorPos e + 1) (length $ editStr e)
                }

deletePreviousChar :: Editor -> Editor
deletePreviousChar e
  | editCursorPos e == 0 = e
  | otherwise = deleteChar $ moveLeft e

gotoBOL :: Editor -> Editor
gotoBOL e = e { editCursorPos = 0 }

gotoEOL :: Editor -> Editor
gotoEOL e = e { editCursorPos = length (editStr e) }

deleteChar :: Editor -> Editor
deleteChar e = e { editStr = s'
                 }
    where
        n = editCursorPos e
        s = editStr e
        s' = take n s <> drop (n+1) s

insertChar :: Char -> Editor -> Editor
insertChar c theEdit = theEdit { editStr = s
                               , editCursorPos = n + 1
                               }
    where
        s = take n oldStr ++ [c] ++ drop n oldStr
        n = editCursorPos theEdit
        oldStr = editStr theEdit

editor :: Name -> String -> Editor
editor name s = Editor s (length s) name

-- edit :: Editor -> Widget
-- edit e =
--     def { render = renderEditor
--         , widgetName = Just $ editorName e
--         }
--     where
--         renderEditor sz@(width, _) attr =
--             let cursorPos = CursorLocation (Location (pos', 0)) (Just $ editorName e)
--                 s = editStr e
--                 pos = editCursorPos e
--                 (s', pos') = let winSize = width
--                                  start = max 0 (pos + 1 - winSize)
--                                  newPos = min pos (width - 1)
--                              in (drop start s, newPos)
--                 w = hBox [ txt s'
--                          , txt (replicate (width - length s' + 1) ' ')
--                          ]
--                 result = render w sz attr
--             in result { renderCursors = [cursorPos]
--                       , renderSizes = []
--                       }
-- 
-- hCentered :: Widget -> Widget
-- hCentered w =
--     def { render = \sz attr ->
--             let result = render w sz attr
--                 img = renderImage result
--                 wOff = (fst sz - imageWidth img) `div` 2
--                 trans = Location (wOff, 0)
--             in result { renderImage = translate wOff 0 img
--                       , renderCursors = (`clOffset` trans) <$> renderCursors result
--                       }
--         }
-- 
-- vCentered :: Widget -> Widget
-- vCentered w =
--     def { render = \sz attr ->
--             let result = render w sz attr
--                 img = renderImage result
--                 hOff = (snd sz - imageHeight img) `div` 2
--                 trans = Location (0, hOff)
--             in result { renderImage = translate 0 hOff img
--                       , renderCursors = (`clOffset` trans) <$> renderCursors result
--                       }
--         }
-- 
-- centered :: Widget -> Widget
-- centered = hCentered . vCentered
-- 
-- centeredAbout :: Location -> Widget -> Widget
-- centeredAbout (Location (col, row)) widget =
--     def { render = \sz@(w, h) attr ->
--             let tx = (w `div` 2) - col
--                 ty = (h `div` 2) - row
--                 result = render widget sz attr
--             in result { renderImage = translate tx ty $ renderImage result
--                       }
--         }

translated :: Location -> Prim -> Prim
translated (Location (wOff, hOff)) p = Translate wOff hOff p

renderFinal :: [Prim]
            -> DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> Picture
renderFinal layerPrims sz chooseCursor = pic
    where
        layerResults = mkImage sz defAttr <$> layerPrims
        pic = picForLayers $ uncurry resize sz <$> layerResults

on :: Color -> Color -> Attr
on f b = defAttr `withForeColor` f
                 `withBackColor` b

fg :: Color -> Attr
fg = (defAttr `withForeColor`)

bg :: Color -> Attr
bg = (defAttr `withBackColor`)

-- withAttr :: Widget -> Attr -> Widget
-- withAttr w attr =
--     def { render = \sz _ -> render w sz attr
--         }
-- 
-- withNamedCursor :: Widget -> (Name, Location) -> Widget
-- withNamedCursor w (name, cursorLoc) =
--     w { render = \sz a -> let result = render w sz a
--                           in result { renderCursors = [CursorLocation cursorLoc (Just name)]
--                                     }
--       }
-- 
-- withCursor :: Widget -> Location -> Widget
-- withCursor w cursorLoc =
--     w { render = \sz a -> let result = render w sz a
--                           in result { renderCursors = [CursorLocation cursorLoc Nothing]
--                                     }
--       }

defaultMain :: App a Event -> a -> IO ()
defaultMain = defaultMainWithVty (mkVty def)

defaultMainWithVty :: IO Vty -> App a Event -> a -> IO ()
defaultMainWithVty buildVty app initialState = do
    chan <- newChan
    withVty buildVty $ \vty -> do
        forkIO $ supplyVtyEvents vty id chan
        runVty vty chan app initialState

supplyVtyEvents :: Vty -> (Event -> e) -> Chan e -> IO ()
supplyVtyEvents vty mkEvent chan =
    forever $ do
        e <- nextEvent vty
        writeChan chan $ mkEvent e

runVty :: Vty -> Chan e -> App a e -> a -> IO ()
runVty vty chan app state = do
    state' <- renderApp vty app state
    e <- readChan chan
    appHandleEvent app e state' >>= runVty vty chan app

withVty :: IO Vty -> (Vty -> IO a) -> IO a
withVty buildVty useVty = do
    vty <- buildVty
    useVty vty `finally` shutdown vty

renderApp :: Vty -> App a e -> a -> IO a
renderApp vty app state = do
    sz <- displayBounds $ outputIface vty
    let pic = renderFinal (appDraw app state) sz (appChooseCursor app state)
    update vty pic

    return state
    -- let !applyResizes = foldl (>>>) id $ (uncurry (appHandleResize app)) <$> sizes
    --     !resizedState = applyResizes state

    -- return resizedState

getNextEvent :: Vty -> App a Event -> a -> IO a
getNextEvent vty app state = do
    e <- nextEvent vty
    appHandleEvent app e state

focusRing :: [Name] -> FocusRing
focusRing [] = FocusRingEmpty
focusRing names = FocusRingNonempty names 0

focusNext :: FocusRing -> FocusRing
focusNext FocusRingEmpty = FocusRingEmpty
focusNext (FocusRingNonempty ns i) = FocusRingNonempty ns i'
    where
        i' = (i + 1) `mod` (length ns)

focusPrev :: FocusRing -> FocusRing
focusPrev FocusRingEmpty = FocusRingEmpty
focusPrev (FocusRingNonempty ns i) = FocusRingNonempty ns i'
    where
        i' = (i + (length ns) - 1) `mod` (length ns)

focusGetCurrent :: FocusRing -> Maybe Name
focusGetCurrent FocusRingEmpty = Nothing
focusGetCurrent (FocusRingNonempty ns i) = Just $ ns !! i

neverShowCursor :: a -> [CursorLocation] -> Maybe CursorLocation
neverShowCursor = const $ const Nothing

showFirstCursor :: a -> [CursorLocation] -> Maybe CursorLocation
showFirstCursor = const $ listToMaybe

focusRingCursor :: (a -> FocusRing) -> a -> [CursorLocation] -> Maybe CursorLocation
focusRingCursor getRing st ls =
    listToMaybe $ filter isCurrent ls
    where
        isCurrent cl = cursorLocationName cl ==
                       (focusGetCurrent $ getRing st)
