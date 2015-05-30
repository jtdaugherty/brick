module Brick.Edit
  ( Editor
  , editor
  , drawEditor
  )
where

import Data.Default
import Data.Monoid ((<>))
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import Brick.Core (Location(..), CursorName(..), HandleEvent(..), SetSize(..))
import Brick.Prim
import Brick.Scroll (HScroll, hScroll, scrollToView)
import Brick.Util (clamp)

data Editor =
    Editor { editStr :: !String
           , editCursorPos :: !Int
           , editorCursorName :: !CursorName
           , editorScroll :: !HScroll
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
    let newCP = clamp 0 (length $ editStr e) pos
    in e { editorScroll = scrollToView (newCP, 1) (editorScroll e)
         , editCursorPos = newCP
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
gotoEOL e = editSetCursorPos (length $ editStr e) e

deleteChar :: Editor -> Editor
deleteChar e = e { editStr = s'
                 }
    where
        n = editCursorPos e
        s = editStr e
        s' = take n s <> drop (n+1) s

insertChar :: Char -> Editor -> Editor
insertChar c theEdit =
    theEdit { editStr = s
            , editCursorPos = newCursorPos
            , editorScroll = scrollToView (newCursorPos, 1) (editorScroll theEdit)
            }
    where
        s = take n oldStr ++ [c] ++ drop n oldStr
        n = editCursorPos theEdit
        newCursorPos = n + 1
        oldStr = editStr theEdit

instance SetSize Editor where
    setSize sz e =
        let updatedScroll = setSize sz $ editorScroll e
        in e { editorScroll = scrollToView (editCursorPos e, 1) updatedScroll
             }

editor :: CursorName -> String -> Editor
editor cName s = Editor s (length s) cName def

drawEditor :: Render Editor
drawEditor =
    saveSize setSize $ hScroll editorScroll $ readState $ \e ->
      let cursorLoc = Location (editCursorPos e, 0)
      in showCursor (editorCursorName e) cursorLoc $ txt (editStr e) <<+ hPad ' '
