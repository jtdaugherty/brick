{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE CPP #-}
-- | This module provides a basic text editor widget. You'll need to
-- embed an 'Editor' in your application state and transform it with
-- 'handleEditorEvent' when relevant events arrive. To get the contents
-- of the editor, just use 'getEditContents'. To modify it, use the
-- 'Z.TextZipper' interface with 'applyEdit'.
--
-- The editor's 'handleEditorEvent' function handles a set of basic
-- input events that should suffice for most purposes; see the source
-- for a complete list.
--
-- Bear in mind that the editor provided by this module is intended to
-- provide basic input support for brick applications but it is not
-- intended to be a replacement for your favorite editor such as Vim or
-- Emacs. It is also not suitable for building sophisticated editors. If
-- you want to build your own editor, I suggest starting from scratch.
module Brick.Widgets.Edit
  ( Editor(editContents, editorName)
  -- * Constructing an editor
  , editor
  , editorText
  -- * Reading editor contents
  , getEditContents
  , getCursorPosition
  -- * Handling events
  , handleEditorEvent
  -- * Editing text
  , applyEdit
  -- * Lenses for working with editors
  , editContentsL
  -- * Rendering editors
  , renderEditor
  -- * Attributes
  , editAttr
  , editFocusedAttr
  -- * UTF-8 decoding of editor pastes
  , DecodeUtf8(..)
  )
where

#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Lens.Micro
import Graphics.Vty (Event(..), Key(..), Modifier(..))

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Zipper as Z hiding ( textZipper )
import qualified Data.Text.Zipper.Generic as Z
import qualified Data.Text.Zipper.Generic.Words as Z
import Data.Tuple (swap)

import Brick.Types
import Brick.Widgets.Core
import Brick.AttrMap

-- | Editor state.  Editors support the following events by default:
--
-- * Mouse clicks: change cursor position
-- * Meta-<: go to beginning of file
-- * Meta->: go to end of file
-- * Ctrl-a, Home: go to beginning of line
-- * Ctrl-e, End: go to end of line
-- * Ctrl-d, Del: delete character at cursor position
-- * Meta-d: delete word at cursor position
-- * Backspace: delete character prior to cursor position
-- * Ctrl-k: delete all from cursor to end of line
-- * Ctrl-u: delete all from cursor to beginning of line
-- * Ctrl-t: transpose character before cursor with the one at cursor position
-- * Meta-b: move one word to the left
-- * Ctrl-b: move one character to the left
-- * Meta-f: move one word to the right
-- * Ctrl-f: move one character to the right
-- * Arrow keys: move cursor
-- * Enter: break the current line at the cursor position
-- * Paste: Bracketed Pastes from the terminal will be pasted, provided
--   the incoming data is UTF-8-encoded.
data Editor t n =
    Editor { editContents :: Z.TextZipper t
           -- ^ The contents of the editor
           , editorName :: n
           -- ^ The name of the editor
           }

suffixLenses ''Editor

instance (Show t, Show n) => Show (Editor t n) where
    show e =
        concat [ "Editor { "
               , "editContents = " <> show (editContents e)
               , ", editorName = " <> show (editorName e)
               , "}"
               ]

instance Named (Editor t n) n where
    getName = editorName

-- | Values that can be constructed by decoding bytestrings in UTF-8
-- encoding.
class DecodeUtf8 t where
    -- | Decode a bytestring assumed to be text in UTF-8 encoding. If
    -- the decoding fails, return 'Left'. This must not raise
    -- exceptions.
    decodeUtf8 :: BS.ByteString -> Either String t

instance DecodeUtf8 T.Text where
    decodeUtf8 bs = case T.decodeUtf8' bs of
        Left e -> Left $ show e
        Right t -> Right t

instance DecodeUtf8 String where
    decodeUtf8 bs = T.unpack <$> decodeUtf8 bs

handleEditorEvent :: (Eq n, DecodeUtf8 t, Eq t, Z.GenericTextZipper t)
                  => BrickEvent n e
                  -> EventM n (Editor t n) ()
handleEditorEvent e = do
    ed <- get
    let f = case e of
              VtyEvent ev ->
                  handleVtyEvent ev
              MouseDown n _ _ (Location pos) | n == getName ed ->
                  Z.moveCursorClosest (swap pos)
              _ -> id
        handleVtyEvent ev = case ev of
            EvPaste bs -> case decodeUtf8 bs of
                Left _ -> id
                Right t -> Z.insertMany t
            EvKey (KChar 'a') [MCtrl] -> Z.gotoBOL
            EvKey (KChar 'e') [MCtrl] -> Z.gotoEOL
            EvKey (KChar 'd') [MCtrl] -> Z.deleteChar
            EvKey (KChar 'd') [MMeta] -> Z.deleteWord
            EvKey (KChar 'k') [MCtrl] -> Z.killToEOL
            EvKey (KChar 'u') [MCtrl] -> Z.killToBOL
            EvKey KEnter [] -> Z.breakLine
            EvKey KDel [] -> Z.deleteChar
            EvKey (KChar c) [] | c /= '\t' -> Z.insertChar c
            EvKey KUp [] -> Z.moveUp
            EvKey KDown [] -> Z.moveDown
            EvKey KLeft [] -> Z.moveLeft
            EvKey KRight [] -> Z.moveRight
            EvKey (KChar 'b') [MCtrl] -> Z.moveLeft
            EvKey (KChar 'f') [MCtrl] -> Z.moveRight
            EvKey (KChar 'b') [MMeta] -> Z.moveWordLeft
            EvKey (KChar 'f') [MMeta] -> Z.moveWordRight
            EvKey KBS [] -> Z.deletePrevChar
            EvKey (KChar 't') [MCtrl] -> Z.transposeChars
            EvKey KHome [] -> Z.gotoBOL
            EvKey KEnd [] -> Z.gotoEOL
            EvKey (KChar '<') [MMeta] -> Z.gotoBOF
            EvKey (KChar '>') [MMeta] -> Z.gotoEOF
            _ -> id
    put $ applyEdit f ed

-- | Construct an editor over 'Text' values
editorText :: n
           -- ^ The editor's name (must be unique)
           -> Maybe Int
           -- ^ The limit on the number of lines in the editor ('Nothing'
           -- means no limit)
           -> T.Text
           -- ^ The initial content
           -> Editor T.Text n
editorText = editor

-- | Construct an editor over 'String' values
editor :: Z.GenericTextZipper a
       => n
       -- ^ The editor's name (must be unique)
       -> Maybe Int
       -- ^ The limit on the number of lines in the editor ('Nothing'
       -- means no limit)
       -> a
       -- ^ The initial content
       -> Editor a n
editor name limit s = Editor (Z.textZipper (Z.lines s) limit) name

-- | Apply an editing operation to the editor's contents.
--
-- This is subject to the restrictions of the underlying text zipper;
-- for example, if the underlying zipper has a line limit configured,
-- any edits applied here will be ignored if they edit text outside
-- the line limit.
applyEdit :: (Z.TextZipper t -> Z.TextZipper t)
          -- ^ The 'Z.TextZipper' editing transformation to apply
          -> Editor t n
          -> Editor t n
applyEdit f e = e & editContentsL %~ f

-- | The attribute assigned to the editor when it does not have focus.
editAttr :: AttrName
editAttr = attrName "edit"

-- | The attribute assigned to the editor when it has focus. Extends
-- 'editAttr'.
editFocusedAttr :: AttrName
editFocusedAttr = editAttr <> attrName "focused"

-- | Get the contents of the editor.
getEditContents :: Monoid t => Editor t n -> [t]
getEditContents e = Z.getText $ e^.editContentsL

-- | Get the cursor position of the editor (row, column).
getCursorPosition :: Editor t n -> (Int, Int)
getCursorPosition e = Z.cursorPosition $ e^.editContentsL

-- | Turn an editor state value into a widget. This uses the editor's
-- name for its scrollable viewport handle and the name is also used to
-- report mouse events.
renderEditor :: (Ord n, Show n, Monoid t, TextWidth t, Z.GenericTextZipper t)
             => ([t] -> Widget n)
             -- ^ The content drawing function
             -> Bool
             -- ^ Whether the editor has focus. It will report a cursor
             -- position if and only if it has focus.
             -> Editor t n
             -- ^ The editor.
             -> Widget n
renderEditor draw foc e =
    let cp = Z.cursorPosition z
        z = e^.editContentsL
        toLeft = Z.take (cp^._2) (Z.currentLine z)
        cursorLoc = Location (textWidth toLeft, cp^._1)
        limit = case e^.editContentsL.to Z.getLineLimit of
            Nothing -> id
            Just lim -> vLimit lim
        atChar = charAtCursor $ e^.editContentsL
        atCharWidth = maybe 1 textWidth atChar
    in withAttr (if foc then editFocusedAttr else editAttr) $
       limit $
       viewport (e^.editorNameL) Both $
       (if foc then showCursor (e^.editorNameL) cursorLoc else id) $
       visibleRegion cursorLoc (atCharWidth, 1) $
       draw $
       getEditContents e

charAtCursor :: (Z.GenericTextZipper t) => Z.TextZipper t -> Maybe t
charAtCursor z =
    let col = snd $ Z.cursorPosition z
        curLine = Z.currentLine z
        toRight = Z.drop col curLine
    in if Z.length toRight > 0
       then Just $ Z.take 1 toRight
       else Nothing
