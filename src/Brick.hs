{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brick where

import Control.Applicative
--import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.Function as DF
import Data.List (sortBy)
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

data Priority = High | Low
              deriving Eq

data Prim = Fixed String
          | HPad Char
          | VPad Char
          | HFill Char
          | VFill Char
          | HBox [(Prim, Priority)]
          | VBox [(Prim, Priority)]
          | HLimit Int Prim
          | VLimit Int Prim
          | UseAttr Attr Prim
          | Raw Image
          | Translate Int Int Prim

instance IsString Prim where
    fromString = Fixed

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
mkImage (w, h) a (HBox pairs) = horizCat $ snd <$> rendered
    where
        pairsIndexed :: [(Int, (Prim, Priority))]
        pairsIndexed = zip [0..] pairs
        his :: [(Int, (Prim, Priority))]
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows :: [(Int, (Prim, Priority))]
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis :: [(Int, Image)]
        renderedHis = (\(i, (prim, _)) -> (i, mkImage (w, h) a prim)) <$> his
        remainingWidth :: Int
        remainingWidth = w - (sum $ (imageWidth . snd) <$> renderedHis)
        widthPerLow :: Int
        widthPerLow = remainingWidth `div` length lows
        heightPerLow = maximum $ (imageHeight . snd) <$> renderedHis
        renderedLows :: [(Int, Image)]
        renderedLows = (\(i, (prim, _)) -> (i, mkImage (widthPerLow, heightPerLow) a prim)) <$> lows
        rendered :: [(Int, Image)]
        rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows
mkImage (w, h) a (VBox pairs) = vertCat $ snd <$> rendered
    where
        pairsIndexed :: [(Int, (Prim, Priority))]
        pairsIndexed = zip [0..] pairs
        his :: [(Int, (Prim, Priority))]
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows :: [(Int, (Prim, Priority))]
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis :: [(Int, Image)]
        renderedHis = (\(i, (prim, _)) -> (i, mkImage (w, h) a prim)) <$> his
        remainingHeight :: Int
        remainingHeight = h - (sum $ (imageHeight . snd) <$> renderedHis)
        heightPerLow :: Int
        heightPerLow = remainingHeight `div` length lows
        widthPerLow = maximum $ (imageWidth . snd) <$> renderedHis
        renderedLows :: [(Int, Image)]
        renderedLows = (\(i, (prim, _)) -> (i, mkImage (widthPerLow, heightPerLow) a prim)) <$> lows
        rendered :: [(Int, Image)]
        rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows

(<+>) :: Prim -> Prim -> Prim
(<+>) a b = HBox [(a, High), (b, High)]

(<<+) :: Prim -> Prim -> Prim
(<<+) a b = HBox [(a, High), (b, Low)]

(+>>) :: Prim -> Prim -> Prim
(+>>) a b = HBox [(a, Low), (b, High)]

(<=>) :: Prim -> Prim -> Prim
(<=>) a b = VBox [(a, High), (b, High)]

(<<=) :: Prim -> Prim -> Prim
(<<=) a b = VBox [(a, High), (b, Low)]

(=>>) :: Prim -> Prim -> Prim
(=>>) a b = VBox [(a, Low), (b, High)]

bordered :: Prim -> Prim
bordered wrapped = total
    where
        topBottom = "+" <<+ HFill '-' +>> "+"
        middle = VFill '|' +>> wrapped <<+ VFill '|'
        total = topBottom =>> middle <<= topBottom

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

hCentered :: Prim -> Prim
hCentered p = HBox [ (HPad ' ', Low)
                   , (p, High)
                   , (HPad ' ', Low)
                   ]

vCentered :: Prim -> Prim
vCentered p = VBox [ (VPad ' ', Low)
                   , (p, High)
                   , (VPad ' ', Low)
                   ]

centered :: Prim -> Prim
centered = vCentered . hCentered

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
