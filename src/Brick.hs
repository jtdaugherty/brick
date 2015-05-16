{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Brick where

import Control.Applicative
import Control.Arrow ((>>>))
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

data Prim = Fixed !String
          | HPad !Char
          | VPad !Char
          | HFill !Char
          | VFill !Char
          | HBox ![(Prim, Priority)]
          | VBox ![(Prim, Priority)]
          | HLimit !Int !Prim
          | VLimit !Int !Prim
          | UseAttr !Attr !Prim
          | Raw !Image
          | Translate !Int !Int !Prim
          | CropLeftBy !Int !Prim
          | CropRightBy !Int !Prim
          | CropTopBy !Int !Prim
          | CropBottomBy !Int !Prim
          | ShowCursor !Name !Location !Prim
          | GetSize !Name !Prim

instance IsString Prim where
    fromString = Fixed

data Render =
    Render { image :: Image
           , cursors :: [CursorLocation]
           , sizes :: [(Name, DisplayRegion)]
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

addCursor :: Name -> Location -> Render -> Render
addCursor n loc r =
    r { cursors = CursorLocation loc (Just n) : cursors r }

addCursorOffset :: Location -> Render -> Render
addCursorOffset off r =
    let onlyVisible = filter isVisible
        isVisible (CursorLocation (Location (w, h)) _) = w >= 0 && h >= 0
    in r { cursors = onlyVisible $ (`clOffset` off) <$> cursors r
         }

for :: [a] -> (a -> b) -> [b]
for = flip map

setImage :: Image -> Render -> Render
setImage i r = r { image = i }

mkImage :: DisplayRegion -> Attr -> Prim -> Render
mkImage (w, h) a (Fixed s) =
    if w > 0 && h > 0
    then Render (crop w h $ string a s) [] []
    else Render emptyImage [] []
mkImage (w, h) _ (Raw img) =
    if w > 0 && h > 0
    then Render (crop w h img) [] []
    else Render emptyImage [] []
mkImage (w, h) a (CropLeftBy c p) =
    let result = mkImage (w, h) a p
        img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropLeft amt img
    in addCursorOffset (Location (-1 * c, 0)) $
       setImage cropped result
mkImage (w, h) a (CropRightBy c p) =
    let result = mkImage (w, h) a p
        img = image result
        amt = imageWidth img - c
        cropped = if amt < 0 then emptyImage else cropRight amt img
    -- xxx cursors
    in setImage cropped result
mkImage (w, h) a (CropTopBy c p) =
    let result = mkImage (w, h) a p
        img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropTop amt img
    in addCursorOffset (Location (0, -1 * c)) $
       setImage cropped result
mkImage (w, h) a (CropBottomBy c p) =
    let result = mkImage (w, h) a p
        img = image result
        amt = imageHeight img - c
        cropped = if amt < 0 then emptyImage else cropBottom amt img
    -- xxx crop cursors
    in setImage cropped result
mkImage (w, h) a (HPad c) = Render (charFill a c w (max 1 h)) [] []
mkImage (w, h) a (VPad c) = Render (charFill a c (max 1 w) h) [] []
mkImage (w, h) a (HFill c) = Render (charFill a c w (min h 1)) [] []
mkImage (w, h) a (VFill c) = Render (charFill a c (min w 1) h) [] []
mkImage (_, h) a (HLimit w p) =
    -- xxx crop cursors
    mkImage (w, h) a p
mkImage (w, _) a (VLimit h p) =
    -- xxx crop cursors
    mkImage (w, h) a p
mkImage (w, h) _ (UseAttr a p) = mkImage (w, h) a p
mkImage (w, h) a (Translate tw th p) =
    let result = mkImage (w, h) a p
        img = image result
    in addCursorOffset (Location (tw, th)) $
       setImage (crop w h $ translate tw th img) result
mkImage sz a (ShowCursor n loc p) =
    let result = mkImage sz a p
    in result { cursors = (CursorLocation loc (Just n)):cursors result }
mkImage sz a (GetSize name p) =
    let result = mkImage sz a p
        img = image result
        imgSz = (imageWidth img, imageHeight img)
    in result { sizes = (name, imgSz) : sizes result
              }
mkImage (w, h) a (HBox pairs) =
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis = (\(i, (prim, _)) -> (i, mkImage (w, h) a prim)) <$> his
        remainingWidth = w - (sum $ (imageWidth . image . snd) <$> renderedHis)
        widthPerLow = remainingWidth `div` length lows
        heightPerLow = maximum $ (imageHeight . image . snd) <$> renderedHis
        renderedLows = (\(i, (prim, _)) -> (i, mkImage (widthPerLow, heightPerLow) a prim)) <$> lows
        rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows

        allResults = snd <$> rendered
        allSizes = sizes <$> allResults
        allImages = image <$> allResults
        allWidths = imageWidth <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (offWidth, 0)
                offWidth = sum $ take i allWidths
            in cursors $ addCursorOffset off result

    in Render (horizCat allImages) (concat allTranslatedCursors) (concat allSizes)

mkImage (w, h) a (VBox pairs) =
    let pairsIndexed = zip [(0::Int)..] pairs
        his = filter (\p -> (snd $ snd p) == High) pairsIndexed
        lows = filter (\p -> (snd $ snd p) == Low) pairsIndexed
        renderedHis = (\(i, (prim, _)) -> (i, mkImage (w, h) a prim)) <$> his
        remainingHeight = h - (sum $ (imageHeight . image . snd) <$> renderedHis)
        heightPerLow = remainingHeight `div` length lows
        widthPerLow = maximum $ (imageWidth . image . snd) <$> renderedHis
        renderedLows = (\(i, (prim, _)) -> (i, mkImage (widthPerLow, heightPerLow) a prim)) <$> lows
        rendered = sortBy (compare `DF.on` fst) $ renderedHis ++ renderedLows

        allResults = snd <$> rendered
        allSizes = sizes <$> allResults
        allImages = image <$> allResults
        allHeights = imageHeight <$> allImages
        allTranslatedCursors = for (zip [0..] allResults) $ \(i, result) ->
            let off = Location (0, offHeight)
                offHeight = sum $ take i allHeights
            in cursors $ addCursorOffset off result

    in Render (vertCat allImages) (concat allTranslatedCursors) (concat allSizes)

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

edit :: Editor -> Prim
edit e = GetSize (Name "edit") $ ShowCursor (Name "edit") (Location (editCursorPos e, 0)) $
         txt (editStr e) <<+ HPad ' '

txt :: String -> Prim
txt = Fixed

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
            -> (Picture, Maybe CursorLocation, [(Name, DisplayRegion)])
renderFinal layerPrims sz chooseCursor = (pic, theCursor, theSizes)
    where
        layerResults = mkImage sz defAttr <$> layerPrims
        pic = picForLayers $ uncurry resize sz <$> image <$> layerResults
        layerCursors = cursors <$> layerResults
        theCursor = chooseCursor $ concat layerCursors
        theSizes = concat $ sizes <$> layerResults

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
runVty vty chan app appState = do
    state' <- renderApp vty app appState
    e <- readChan chan
    appHandleEvent app e state' >>= runVty vty chan app

withVty :: IO Vty -> (Vty -> IO a) -> IO a
withVty buildVty useVty = do
    vty <- buildVty
    useVty vty `finally` shutdown vty

renderApp :: Vty -> App a e -> a -> IO a
renderApp vty app appState = do
    sz <- displayBounds $ outputIface vty
    let (pic, theCursor, theSizes) = renderFinal (appDraw app appState) sz (appChooseCursor app appState)
        picWithCursor = case theCursor of
            Nothing -> pic { picCursor = NoCursor }
            Just (CursorLocation (Location (w, h)) _) -> pic { picCursor = Cursor w h }

    update vty picWithCursor

    let !applyResizes = foldl (>>>) id $ (uncurry (appHandleResize app)) <$> theSizes
        !resizedState = applyResizes appState

    return resizedState

getNextEvent :: Vty -> App a Event -> a -> IO a
getNextEvent vty app appState = do
    e <- nextEvent vty
    appHandleEvent app e appState

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
