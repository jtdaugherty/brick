{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This module provides a basic single-line text editor widget. You'll
-- need to embed an 'Editor' in your application state and transform it
-- with 'handleEvent' when relevant events arrive. To get the contents
-- of the editor, just use 'editContents'.
--
-- The editor's 'HandleEvent' instance handles a set of basic input
-- events that should suffice for most purposes; see the source for a
-- complete list.
module Brick.Widgets.Edit
  ( Editor(editContents, editCursorPos, editorName, editDrawContents)
  -- * Constructing an editor
  , editor
  -- * Lenses for working with editors
  , editContentsL
  , editCursorPosL
  , editDrawContentsL
  -- * Rendering editors
  , renderEditor
  -- * Attributes
  , editAttr
  )
where

import Control.Lens
import Data.Monoid ((<>))
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import Brick.Types
import Brick.Widgets.Core
import Brick.Util (clamp)
import Brick.AttrMap

-- | Editor state.
data Editor =
    Editor { editContents :: !String
           -- ^ The contents of the editor
           , editCursorPos :: !Int
           -- ^ The editor's cursor position
           , editDrawContents :: String -> Widget
           -- ^ The function the editor uses to draw its contents
           , editorName :: Name
           -- ^ The name of the editor
           }

suffixLenses ''Editor

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
    let newCP = clamp 0 (length $ e^.editContentsL) pos
    in e & editCursorPosL .~ newCP

moveLeft :: Editor -> Editor
moveLeft e = editSetCursorPos (e^.editCursorPosL - 1) e

moveRight :: Editor -> Editor
moveRight e = editSetCursorPos (e^.editCursorPosL + 1) e

deletePreviousChar :: Editor -> Editor
deletePreviousChar e
  | e^.editCursorPosL == 0 = e
  | otherwise = deleteChar $ moveLeft e

gotoBOL :: Editor -> Editor
gotoBOL = editSetCursorPos 0

gotoEOL :: Editor -> Editor
gotoEOL e = editSetCursorPos (length $ e^.editContentsL) e

deleteChar :: Editor -> Editor
deleteChar e = e & editContentsL %~ listRemove (e^.editCursorPosL)

listRemove :: Int -> [a] -> [a]
listRemove i as
  | i >= 0 && i < length as = take i as <> drop (i + 1) as
  | otherwise = as

insertChar :: Char -> Editor -> Editor
insertChar c theEdit =
    theEdit & editContentsL %~ listInsert c (theEdit^.editCursorPosL)
            & editCursorPosL %~ (+ 1)

listInsert :: a -> Int -> [a] -> [a]
listInsert a i as = take i as ++ [a] ++ drop i as

-- | Construct an editor.
editor :: Name
       -- ^ The editor's name (must be unique)
       -> (String -> Widget)
       -- ^ The content rendering function
       -> String
       -- ^ The initial content
       -> Editor
editor name draw s = Editor s (length s) draw name

-- | The attribute assigned to the editor
editAttr :: AttrName
editAttr = "edit"

-- | Turn an editor state value into a widget
renderEditor :: Editor -> Widget
renderEditor e =
    let cursorLoc = Location (e^.editCursorPosL, 0)
    in withAttr editAttr $
       vLimit 1 $
       viewport (e^.editorNameL) Horizontal $
       showCursor (e^.editorNameL) cursorLoc $
       visibleRegion cursorLoc (1, 1) $
       e^.editDrawContentsL $
       e^.editContentsL
