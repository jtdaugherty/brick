{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Brick where

import Control.Applicative hiding ((<|>))
import Control.Arrow ((>>>))
import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forever)
import Data.Default
import Data.Maybe
import Data.String
import Data.Monoid
import Graphics.Vty

newtype Location = Location (Int, Int)

origin :: Location
origin = Location (0, 0)

newtype Name = Name String
               deriving (Eq, Show)

data CursorLocation =
    CursorLocation { cursorLocation :: !Location
                   , cursorLocationName :: !(Maybe Name)
                   }

data Render =
    Render { renderImage :: !Image
           , renderCursors :: ![CursorLocation]
           , renderSizes :: ![(Name, DisplayRegion)]
           }

instance Default Render where
    def = Render emptyImage [] []

data Widget =
    Widget { render :: DisplayRegion -> Attr -> Render
           , widgetName :: !(Maybe Name)
           }

instance IsString Widget where
    fromString = txt

data Editor =
    Editor { editStr :: !String
           , editCursorPos :: !Int
           , editorName :: !Name
           }

instance Default Widget where
    def = Widget { render = const $ const def
                 , widgetName = Nothing
                 }

data App a e =
    App { appDraw :: a -> [Widget]
        , appChooseCursor :: a -> [CursorLocation] -> Maybe CursorLocation
        , appHandleEvent :: e -> a -> IO a
        , appHandleResize :: Name -> DisplayRegion -> a -> a
        }

instance Default (App a e) where
    def = App { appDraw = const def
              , appChooseCursor = const $ const Nothing
              , appHandleEvent = const return
              , appHandleResize = const $ const id
              }

data FocusRing = FocusRingEmpty
               | FocusRingNonempty ![Name] !Int

locOffset :: Location -> Location -> Location
locOffset (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

clOffset :: CursorLocation -> Location -> CursorLocation
clOffset cl loc = cl { cursorLocation = (cursorLocation cl) `locOffset` loc }

bordered :: Widget -> Widget
bordered w =
    def { render = renderBordered w
        }

renderBordered :: Widget -> DisplayRegion -> Attr -> Render
renderBordered w sz attr = result { renderImage = borderedImg
                                  , renderCursors = translatedCursors
                                  }
    where
        result = render w sz attr
        childImg = renderImage result
        (width, height) = ( imageWidth childImg
                          , imageHeight childImg
                          )
        topBottomBorder = horizCat [ string attr "+"
                                   , charFill attr '-' width 1
                                   , string attr "+"
                                   ]
        leftRightBorder = charFill attr '|' 1 height
        withSideBorders = horizCat [ leftRightBorder
                                   , childImg
                                   , leftRightBorder
                                   ]
        borderedImg = vertCat [ topBottomBorder
                              , withSideBorders
                              , topBottomBorder
                              ]
        translatedCursors = (`clOffset` (Location (1,1))) <$>
                            renderCursors result

txt :: String -> Widget
txt s =
    def { render = \_ a -> def { renderImage = string a s }
    }

named :: Widget -> Name -> Widget
named w name = w { widgetName = Just name }

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

edit :: Editor -> Widget
edit e =
    def { render = renderEditor
        , widgetName = Just $ editorName e
        }
    where
        renderEditor sz@(width, _) attr =
            let cursorPos = CursorLocation (Location (pos', 0)) (Just $ editorName e)
                s = editStr e
                pos = editCursorPos e
                (s', pos') = let winSize = width
                                 start = max 0 (pos + 1 - winSize)
                                 newPos = min pos (width - 1)
                             in (drop start s, newPos)
                w = hBox [ txt s'
                         , txt (replicate (width - length s' + 1) ' ')
                         ]
                result = render w sz attr
            in result { renderCursors = [cursorPos]
                      , renderSizes = []
                      }

hBorder :: Char -> Widget
hBorder ch =
    def { render = \(width, _) attr ->
            def { renderImage = charFill attr ch width 1 }
        }

vBorder :: Char -> Widget
vBorder ch =
    def { render = \(_, height) attr ->
            def { renderImage = charFill attr ch 1 height }
        }

vBox :: [Widget] -> Widget
vBox widgets =
    def { render = renderVBox
        }
    where
        renderVBox (width, height) attr =
            let results = renderChildren attr width widgets height origin
                imgs = renderImage <$> results
                maxWidth = maximum $ imageWidth <$> imgs
                padded = addPadding maxWidth attr <$> imgs
            in def { renderImage = vertCat padded
                   , renderCursors = concat $ renderCursors <$> results
                   , renderSizes = concat $ renderSizes <$> results
                   }

        addPadding width attr img =
            img <|> charFill attr ' ' (width - imageWidth img) (imageHeight img)

        renderChildren _ _ [] _ _ = []
        renderChildren attr width (w:ws) hRemaining loc
          | hRemaining <= 0 = []
          | otherwise =
            let result = render_ w loc (width, hRemaining) attr
                img = renderImage result
                newHeight = hRemaining - imageHeight img
                newLoc = loc `locOffset` Location (0, imageHeight img)
                results = renderChildren attr width ws newHeight newLoc
            in result:results

hBox :: [Widget] -> Widget
hBox widgets =
    def { render = renderHBox
        }
    where
        renderHBox (width, height) attr =
            let results = renderChildren attr height widgets width origin
                imgs = renderImage <$> results
                maxHeight = maximum $ imageHeight <$> imgs
                padded = addPadding maxHeight attr <$> imgs
            in def { renderImage = horizCat padded
                   , renderCursors = concat $ renderCursors <$> results
                   , renderSizes = concat $ renderSizes <$> results
                   }

        addPadding height attr img =
            img <-> charFill attr ' ' (imageWidth img) (height - imageHeight img)

        renderChildren _ _ [] _ _ = []
        renderChildren attr height (w:ws) wRemaining loc
          | wRemaining <= 0 = []
          | otherwise =
            let result = render_ w loc (wRemaining, height) attr
                img = renderImage result
                newWidth = wRemaining - imageWidth img
                newLoc = loc `locOffset` Location (imageWidth img, 0)
                results = renderChildren attr height ws newWidth newLoc
            in result:results

hLimit :: Int -> Widget -> Widget
hLimit width w =
    def { render = \(_, height) attr -> render w (width, height) attr
        }

vLimit :: Int -> Widget -> Widget
vLimit height w =
    def { render = \(width, _) attr -> render w (width, height) attr
        }

hCentered :: Widget -> Widget
hCentered w =
    def { render = \sz attr ->
            let result = render w sz attr
                img = renderImage result
                wOff = (fst sz - imageWidth img) `div` 2
                trans = Location (wOff, 0)
            in result { renderImage = translate wOff 0 img
                      , renderCursors = (`clOffset` trans) <$> renderCursors result
                      }
        }

vCentered :: Widget -> Widget
vCentered w =
    def { render = \sz attr ->
            let result = render w sz attr
                img = renderImage result
                hOff = (snd sz - imageHeight img) `div` 2
                trans = Location (0, hOff)
            in result { renderImage = translate 0 hOff img
                      , renderCursors = (`clOffset` trans) <$> renderCursors result
                      }
        }

centered :: Widget -> Widget
centered = hCentered . vCentered

translated :: Location -> Widget -> Widget
translated off@(Location (wOff, hOff)) w =
    def { render = \sz attr ->
            let result = render w sz attr
            in result { renderImage = translate wOff hOff $ renderImage result
                      , renderCursors = (`clOffset` off) <$> renderCursors result
                      }
        }

render_ :: Widget -> Location -> DisplayRegion -> Attr -> Render
render_ w loc sz attr =
    def { renderImage = uncurry crop sz img
        , renderSizes = case widgetName w of
            Nothing -> renderSizes result
            Just n -> (n, (imageWidth img, imageHeight img)) : renderSizes result
        , renderCursors = (`clOffset` loc) <$> renderCursors result
        }
    where
        result = render w sz attr
        img = renderImage result

renderFinal :: [Widget]
            -> DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> (Picture, [(Name, DisplayRegion)])
renderFinal layerWidgets sz chooseCursor = (pic, concatMap renderSizes layerResults)
    where
        cursor = case chooseCursor (concatMap renderCursors layerResults) of
                   Nothing -> NoCursor
                   Just cl -> let Location (w, h) = cursorLocation cl
                              in Cursor w h
        layerRenderResult w = render_ w (Location (0, 0)) sz defAttr
        layerResults = layerRenderResult <$> layerWidgets
        basePic = picForLayers $ uncurry resize sz <$> renderImage <$> layerResults
        pic = basePic { picCursor = cursor }

liftVty :: Image -> Widget
liftVty img =
    def { render = const $ const $ def { renderImage = img }
        }

on :: Color -> Color -> Attr
on f b = defAttr `withForeColor` f
                 `withBackColor` b

fg :: Color -> Attr
fg = (defAttr `withForeColor`)

bg :: Color -> Attr
bg = (defAttr `withBackColor`)

withAttr :: Widget -> Attr -> Widget
withAttr w attr =
    def { render = \sz _ -> render w sz attr
        }

withNamedCursor :: Widget -> (Name, Location) -> Widget
withNamedCursor w (name, cursorLoc) =
    w { render = \sz a -> let result = render w sz a
                          in result { renderCursors = [CursorLocation cursorLoc (Just name)]
                                    }
      }

withCursor :: Widget -> Location -> Widget
withCursor w cursorLoc =
    w { render = \sz a -> let result = render w sz a
                          in result { renderCursors = [CursorLocation cursorLoc Nothing]
                                    }
      }

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
    let (pic, sizes) = renderFinal (appDraw app state) sz (appChooseCursor app state)
    update vty pic

    let !applyResizes = foldl (>>>) id $ (uncurry (appHandleResize app)) <$> sizes
        !resizedState = applyResizes state

    return resizedState

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

focusRingCursor :: (a -> FocusRing) -> a -> [CursorLocation] -> Maybe CursorLocation
focusRingCursor getRing st ls =
    listToMaybe $ filter isCurrent ls
    where
        isCurrent cl = cursorLocationName cl ==
                       (focusGetCurrent $ getRing st)
