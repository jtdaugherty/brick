{-# LANGUAGE BangPatterns #-}
module Brick where

import Control.Monad.IO.Class
import Control.Applicative
import Control.Arrow ((>>>))
import Data.Default
import Data.Maybe
import Data.String
import Data.Monoid
import System.Exit
import Graphics.Vty

newtype Location = Location (Int, Int)

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
    Widget { render :: Location -> DisplayRegion -> Attr -> Render
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
    def = Widget { render = const $ const $ const def
                 , widgetName = Nothing
                 }

data App a =
    App { appDraw :: a -> Widget
        , appChooseCursor :: a -> [CursorLocation] -> Maybe CursorLocation
        , appHandleEvent :: Event -> a -> Either ExitCode a
        , appHandleResize :: Name -> DisplayRegion -> a -> a
        }

instance Default (App a) where
    def = App { appDraw = const def
              , appChooseCursor = const $ const Nothing
              , appHandleEvent = const $ Right
              , appHandleResize = const $ const id
              }

data FocusRing = FocusRingEmpty
               | FocusRingNonempty ![Name] !Int

locOffset :: Location -> Location -> Location
locOffset (Location (w1, h1)) (Location (w2, h2)) = Location (w1+w2, h1+h2)

clOffset :: CursorLocation -> Location -> CursorLocation
clOffset cl loc = cl { cursorLocation = (cursorLocation cl) `locOffset` loc }

txt :: String -> Widget
txt s =
    def { render = \_ _ a -> def { renderImage = string a s }
    }

named :: Widget -> Name -> Widget
named w name = w { widgetName = Just name }

editEvent :: Event -> Editor -> Editor
editEvent e theEdit = f theEdit
    where
        f = case e of
              EvKey (KChar 'a') [MCtrl] -> gotoBOL
              EvKey (KChar 'e') [MCtrl] -> gotoEOL
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
        renderEditor loc sz@(width, _) attr =
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
                result = render_ w loc sz attr
            in result { renderCursors = [cursorPos]
                      , renderSizes = []
                      }

hBorder :: Char -> Widget
hBorder ch =
    def { render = \_ (width, _) attr ->
            def { renderImage = charFill attr ch width 1 }
        }

vBorder :: Char -> Widget
vBorder ch =
    def { render = \_ (_, height) attr ->
            def { renderImage = charFill attr ch 1 height }
        }

vBox :: [Widget] -> Widget
vBox widgets =
    def { render = renderVBox
        }
    where
        renderVBox loc (width, height) attr =
            let results = doIt attr width widgets height loc
            in def { renderImage = vertCat $ renderImage <$> results
                   , renderCursors = concat $ renderCursors <$> results
                   , renderSizes = concat $ renderSizes <$> results
                   }

        doIt _ _ [] _ _ = []
        doIt attr width (w:ws) hRemaining loc
          | hRemaining <= 0 = []
          | otherwise =
            let result = render w loc (width, hRemaining) attr
                img = renderImage result
                newHeight = hRemaining - imageHeight img
                newLoc = loc `locOffset` Location (0, imageHeight img)
                results = doIt attr width ws newHeight newLoc
            in result:results

hBox :: [Widget] -> Widget
hBox widgets =
    def { render = renderHBox
        }
    where
        renderHBox loc (width, height) attr =
            let results = doIt attr height widgets width loc
            in def { renderImage = horizCat $ renderImage <$> results
                   , renderCursors = concat $ renderCursors <$> results
                   , renderSizes = concat $ renderSizes <$> results
                   }

        doIt _ _ [] _ _ = []
        doIt attr height (w:ws) wRemaining loc
          | wRemaining <= 0 = []
          | otherwise =
            let result = render w loc (wRemaining, height) attr
                img = renderImage result
                newWidth = wRemaining - imageWidth img
                newLoc = loc `locOffset` Location (imageWidth img, 0)
                results = doIt attr height ws newWidth newLoc
            in result:results

hLimit :: Int -> Widget -> Widget
hLimit width w =
    def { render = \loc (_, height) attr -> render_ w loc (width, height) attr
        }

vLimit :: Int -> Widget -> Widget
vLimit height w =
    def { render = \loc (width, _) attr -> render_ w loc (width, height) attr
        }

render_ :: Widget -> Location -> DisplayRegion -> Attr -> Render
render_ w loc sz attr =
    result { renderImage = uncurry crop sz img
           , renderSizes = case widgetName w of
               Nothing -> renderSizes result
               Just n -> (n, (imageWidth img, imageHeight img)) : renderSizes result
           }
    where
        result = render w loc sz attr
        img = renderImage result

renderFinal :: Widget
            -> DisplayRegion
            -> ([CursorLocation] -> Maybe CursorLocation)
            -> (Picture, [(Name, DisplayRegion)])
renderFinal widget sz chooseCursor = (pic, renderSizes renderResult)
    where
        pic = basePic { picCursor = cursor }
        basePic = picForImage $ uncurry resize sz $ renderImage renderResult
        cursor = case chooseCursor (renderCursors renderResult) of
                   Nothing -> NoCursor
                   Just cl -> let Location (w, h) = cursorLocation cl
                              in Cursor w h
        renderResult = render_ widget (Location (0, 0)) sz defAttr

liftVty :: Image -> Widget
liftVty img =
    def { render = const $ const $ const $ def { renderImage = img }
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
    def { render = \loc sz _ -> render_ w loc sz attr
        }

withNamedCursor :: Widget -> (Name, Location) -> Widget
withNamedCursor w (name, cursorLoc) =
    w { render = \loc sz a -> let result = render_ w loc sz a
                              in result { renderCursors = [CursorLocation (cursorLoc `locOffset` loc) (Just name)]
                                        }
      }

withCursor :: Widget -> Location -> Widget
withCursor w cursorLoc =
    w { render = \loc sz a -> let result = render_ w loc sz a
                              in result { renderCursors = [CursorLocation (cursorLoc `locOffset` loc) Nothing]
                                        }
      }

runVty :: (MonadIO m) => App a -> a -> Vty -> m ()
runVty app initialState vty = do
    let run state = do
          sz <- liftIO $ displayBounds $ outputIface vty
          let (pic, sizes) = renderFinal (appDraw app state) sz (appChooseCursor app state)
          liftIO $ update vty pic

          let applyResizes = foldl (>>>) id $ (uncurry (appHandleResize app)) <$> sizes
              resizedState = applyResizes state

          e <- liftIO $ nextEvent vty
          case appHandleEvent app e resizedState of
              Left status -> liftIO $ do
                  shutdown vty
                  exitWith status
              Right newState -> run newState

    run initialState

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
