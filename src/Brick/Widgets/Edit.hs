{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.Edit
  ( Editor(editContents)
  , editor
  , renderEditor
  , editAttr
  )
where

import Data.Monoid ((<>))
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import Brick.Core (Name, Location(..), HandleEvent(..))
import Brick.Widgets.Core
import Brick.Util (clamp)
import Brick.AttrMap

data Editor =
    Editor { editContents :: !String
           , editCursorPos :: !Int
           , editDrawContents :: String -> Widget
           , editorName :: Name
           }

instance HandleEvent Editor where
    handleEvent e =
        case e of
            EvKey (KChar 'a') [MCtrl] -> gotoBOL
            EvKey (KChar 'e') [MCtrl] -> gotoEOL
            EvKey (KChar 'd') [MCtrl] -> deleteChar
            EvKey (KChar c) [] | c /= '\t' -> insertChar c
            EvKey KDel [] -> deleteChar
            EvKey KLeft [] -> moveLeft
            EvKey KRight [] -> moveRight
            EvKey KBS [] -> deletePreviousChar
            _ -> id

editSetCursorPos :: Int -> Editor -> Editor
editSetCursorPos pos e =
    let newCP = clamp 0 (length $ editContents e) pos
    in e { editCursorPos = newCP
         }

moveLeft :: Editor -> Editor
moveLeft e = editSetCursorPos (editCursorPos e - 1) e

moveRight :: Editor -> Editor
moveRight e = editSetCursorPos (editCursorPos e + 1) e

deletePreviousChar :: Editor -> Editor
deletePreviousChar e
  | editCursorPos e == 0 = e
  | otherwise = deleteChar $ moveLeft e

gotoBOL :: Editor -> Editor
gotoBOL = editSetCursorPos 0

gotoEOL :: Editor -> Editor
gotoEOL e = editSetCursorPos (length $ editContents e) e

deleteChar :: Editor -> Editor
deleteChar e = e { editContents = s'
                 }
    where
        n = editCursorPos e
        s = editContents e
        s' = take n s <> drop (n+1) s

insertChar :: Char -> Editor -> Editor
insertChar c theEdit =
    theEdit { editContents = s
            , editCursorPos = newCursorPos
            }
    where
        s = take n oldStr ++ [c] ++ drop n oldStr
        n = editCursorPos theEdit
        newCursorPos = n + 1
        oldStr = editContents theEdit

editor :: Name -> (String -> Widget) -> String -> Editor
editor name draw s = Editor s (length s) draw name

editAttr :: AttrName
editAttr = "edit"

renderEditor :: Editor -> Widget
renderEditor e =
    let cursorLoc = Location (editCursorPos e, 0)
    in withAttrName editAttr $
       vLimit 1 $
       viewport (editorName e) Horizontal $
       showCursor (editorName e) cursorLoc $
       visibleRegion cursorLoc (1, 1) $
       editDrawContents e $
       editContents e
