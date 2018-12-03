{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | This module provids a file browser widget that allows users to
-- navigate directory trees, search for files and directories, and
-- select a file of interest.
--
-- To use this module:
--
-- * Embed a 'FileBrowser' in your application state.
-- * Dispatch events to it in your event handler with
--   'handleFileBrowserEvent'.
-- * Get the entry under the browser's cursor with 'fileBrowserCursor'
--   and get the entry selected by the user with 'Enter' using
--   'fileBrowserSelection'.
--
-- File browsers have a built-in user-configurable function to limit the
-- entries displayed that defaults to showing all files. For example,
-- an application might want to limit the browser to just directories
-- and XML files. That is accomplished by setting the filter with
-- 'setFileBrowserEntryFilter' and some examples are provided in this
-- module: 'fileTypeMatch' and 'fileExtensionMatch'.
--
-- File browsers are styled using the provided collection of attribute
-- names, so add those to your attribute map to get the appearance you
-- want. File browsers also make use of a 'List' internally, so the
-- 'List' attributes will affect how the list appears.
--
-- File browsers catch 'IOException's when changing directories. If a
-- call to 'setWorkingDirectory' triggers an 'IOException' while reading
-- the working directory, the resulting 'IOException' is stored in the
-- file browser and is accessible with 'fileBrowserException'. The
-- 'setWorkingDirectory' function clears the exception field if the
-- working directory is read successfully. The caller is responsible for
-- deciding when and whether to display the exception to the user. In
-- the event that an 'IOException' is raised as described here, the file
-- browser will always present @..@ as a navigation option to allow the
-- user to continue navigating up the directory tree. It does this even
-- if the current or parent directory does not exist or cannot be read,
-- so it is always safe to present a file browser for any working
-- directory. Bear in mind that the @..@ entry is always subjected to
-- filtering and searching.
module Brick.Widgets.FileBrowser
  ( -- * Types
    FileBrowser
  , FileInfo(..)
  , FileType(..)
  -- * Making a new file browser
  , newFileBrowser

  -- * Manipulating a file browser's state
  , setWorkingDirectory
  , getWorkingDirectory
  , updateFileBrowserSearch
  , setFileBrowserEntryFilter

  -- * Handling events
  , handleFileBrowserEvent

  -- * Rendering
  , renderFileBrowser

  -- * Getting information
  , fileBrowserCursor
  , fileBrowserIsSearching
  , fileBrowserSelection
  , fileBrowserException

  -- * Attributes
  , fileBrowserAttr
  , fileBrowserCurrentDirectoryAttr
  , fileBrowserSelectionInfoAttr
  , fileBrowserDirectoryAttr
  , fileBrowserBlockDeviceAttr
  , fileBrowserRegularFileAttr
  , fileBrowserCharacterDeviceAttr
  , fileBrowserNamedPipeAttr
  , fileBrowserSymbolicLinkAttr
  , fileBrowserUnixSocketAttr

  -- * Example browser entry filters
  , fileTypeMatch
  , fileExtensionMatch

  -- * Lenses
  , fileBrowserEntryFilterL
  , fileInfoFilenameL
  , fileInfoFileSizeL
  , fileInfoSanitizedFilenameL
  , fileInfoFilePathL
  , fileInfoFileTypeL

  -- * Miscellaneous
  , prettyFileSize

  -- * Utilities
  , entriesForDirectory
  )
where

import qualified Control.Exception as E
import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower, isPrint)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Int (Int64)
import Data.List (sortBy, isSuffixOf)
import qualified Data.Vector as V
import Lens.Micro
import qualified Graphics.Vty as Vty
import qualified System.Directory as D
import qualified System.Posix.Files as U
import qualified System.Posix.Types as U
import qualified System.FilePath as FP
import Text.Printf (printf)

import Brick.Types
import Brick.AttrMap (AttrName)
import Brick.Widgets.Core
import Brick.Widgets.List

-- | A file browser's state. Embed this in your application state and
-- transform it with 'handleFileBrowserEvent' and the functions included
-- in this module.
data FileBrowser n =
    FileBrowser { fileBrowserWorkingDirectory :: FilePath
                , fileBrowserEntries :: List n FileInfo
                , fileBrowserLatestResults :: [FileInfo]
                , fileBrowserName :: n
                , fileBrowserSelectionState :: Maybe FileInfo
                , fileBrowserEntryFilter :: Maybe (FileInfo -> Bool)
                , fileBrowserSearchString :: Maybe T.Text
                , fileBrowserException :: Maybe E.IOException
                }

-- | Information about a file entry in the browser.
data FileInfo =
    FileInfo { fileInfoFilename :: String
             -- ^ The filename of this entry, without its path.
             -- This is not for display purposes; for that, use
             -- 'fileInfoSanitizedFilename'.
             , fileInfoSanitizedFilename :: String
             -- ^ The filename of this entry with out its path,
             -- sanitized of non-printable characters (replaced with
             -- '?'). This is for display purposes only.
             , fileInfoFilePath :: FilePath
             -- ^ The full path to this entry's file.
             , fileInfoFileType :: Maybe FileType
             -- ^ The type of this entry's file, if it could be
             -- determined.
             , fileInfoFileSize :: Int64
             -- ^ The size, in bytes, of this entry's file.
             }
             deriving (Show, Eq, Read)

-- | The type of file entries in the browser.
data FileType =
    RegularFile
    -- ^ A regular disk file.
    | BlockDevice
    -- ^ A block device.
    | CharacterDevice
    -- ^ A character device.
    | NamedPipe
    -- ^ A named pipe.
    | Directory
    -- ^ A directory.
    | SymbolicLink
    -- ^ A symbolic link.
    | UnixSocket
    -- ^ A Unix socket.
    deriving (Read, Show, Eq)

suffixLenses ''FileBrowser
suffixLenses ''FileInfo

-- | Make a new file browser state. The provided resource name will be
-- used to render the 'List' viewport of the browser.
--
-- By default, the browser will show all files and directories
-- in its working directory. To change that behavior, see
-- 'setFileBrowserEntryFilter'.
newFileBrowser :: n
               -- ^ The resource name associated with the browser's
               -- entry listing.
               -> Maybe FilePath
               -- ^ The initial working directory that the browser
               -- displays. If not provided, this defaults to the
               -- executable's current working directory.
               -> IO (FileBrowser n)
newFileBrowser name mCwd = do
    initialCwd <- case mCwd of
        Just path -> return path
        Nothing -> D.getCurrentDirectory

    let b = FileBrowser { fileBrowserWorkingDirectory = initialCwd
                        , fileBrowserEntries = list name mempty 1
                        , fileBrowserLatestResults = mempty
                        , fileBrowserName = name
                        , fileBrowserSelectionState = Nothing
                        , fileBrowserEntryFilter = Nothing
                        , fileBrowserSearchString = Nothing
                        , fileBrowserException = Nothing
                        }

    setWorkingDirectory initialCwd b

-- | Set the filtering function used to determine which entries in
-- the browser's current directory appear in the browser. 'Nothing'
-- indicates no filtering, meaning all entries will be shown. 'Just'
-- indicates a function that should return 'True' for entries that
-- should be permitted to appear.
setFileBrowserEntryFilter :: Maybe (FileInfo -> Bool) -> FileBrowser n -> FileBrowser n
setFileBrowserEntryFilter f b =
    applyFilterAndSearch $ b & fileBrowserEntryFilterL .~ f

-- | Set the working directory of the file browser. This scans the new
-- directory and repopulates the browser while maintaining any active
-- search string and/or entry filtering.
--
-- If the directory scan raises an 'IOException', the exception is
-- stored the browser and is accessible with 'fileBrowserException'. If
-- no exception is raised, the exception field is cleared. Regardless of
-- whether an exception is raised, @..@ is always presented as a valid
-- option in the browser.
setWorkingDirectory :: FilePath -> FileBrowser n -> IO (FileBrowser n)
setWorkingDirectory path b = do
    entriesResult <- E.try $ entriesForDirectory path

    let (entries, exc) = case entriesResult of
            Left (e::E.IOException) -> ([], Just e)
            Right es -> (es, Nothing)

    allEntries <- if path == "/" then return entries else do
        parentResult <- E.try $ parentOf path
        return $ case parentResult of
            Left (_::E.IOException) -> entries
            Right parent -> parent : entries

    let b' = setEntries allEntries b
    return $ b' & fileBrowserWorkingDirectoryL .~ path
                & fileBrowserExceptionL .~ exc

parentOf :: FilePath -> IO FileInfo
parentOf path = do
    filePath <- D.canonicalizePath $ FP.takeDirectory path
    status <- U.getFileStatus filePath
    let U.COff sz = U.fileSize status
    return FileInfo { fileInfoFilename = ".."
                    , fileInfoFilePath = filePath
                    , fileInfoSanitizedFilename = ".."
                    , fileInfoFileType = fileTypeFromStatus status
                    , fileInfoFileSize = sz
                    }

-- | Get the working directory of the file browser.
getWorkingDirectory :: FileBrowser n -> FilePath
getWorkingDirectory = fileBrowserWorkingDirectory

setEntries :: [FileInfo] -> FileBrowser n -> FileBrowser n
setEntries es b =
    applyFilterAndSearch $ b & fileBrowserLatestResultsL .~ es

-- | Returns whether the file browser is in search mode, i.e., the mode
-- in which user input affects the browser's active search string and
-- displayed entries. This is used to aid in event dispatching in the
-- calling program.
fileBrowserIsSearching :: FileBrowser n -> Bool
fileBrowserIsSearching b = isJust $ b^.fileBrowserSearchStringL

-- | Get the entry chosen by the user, if any. The entry is chosen
-- by an 'Enter' keypress; if you want the entry under the cursor, use
-- 'fileBrowserCursor'.
fileBrowserSelection :: FileBrowser n -> Maybe FileInfo
fileBrowserSelection = fileBrowserSelectionState

-- | Modify the file browser's active search string. This causes the
-- browser's displayed entries to change to those in its current
-- directory that match the search string, if any. If a search string
-- is provided, it is matched case-insensitively anywhere in file or
-- directory names.
updateFileBrowserSearch :: (Maybe T.Text -> Maybe T.Text)
                        -- ^ The search transformation. 'Nothing'
                        -- indicates that search mode should be off;
                        -- 'Just' indicates that it should be on and
                        -- that the provided search string should be
                        -- used.
                        -> FileBrowser n
                        -- ^ The browser to modify.
                        -> FileBrowser n
updateFileBrowserSearch f b =
    let old = b^.fileBrowserSearchStringL
        new = f $ b^.fileBrowserSearchStringL
        oldLen = maybe 0 T.length old
        newLen = maybe 0 T.length new
    in if old == new
       then b
       else if oldLen == newLen
            -- This case avoids a list rebuild and cursor position reset
            -- when the search state isn't *really* changing.
            then b & fileBrowserSearchStringL .~ new
            else applyFilterAndSearch $ b & fileBrowserSearchStringL .~ new

applyFilterAndSearch :: FileBrowser n -> FileBrowser n
applyFilterAndSearch b =
    let filterMatch = fromMaybe (const True) (b^.fileBrowserEntryFilterL)
        searchMatch = maybe (const True)
                            (\search i -> (T.toLower search `T.isInfixOf` (T.pack $ toLower <$> fileInfoSanitizedFilename i)))
                            (b^.fileBrowserSearchStringL)
        match i = filterMatch i && searchMatch i
        matching = filter match $ b^.fileBrowserLatestResultsL
    in b { fileBrowserEntries = list (b^.fileBrowserNameL) (V.fromList matching) 1 }

-- | Generate a textual abbreviation of a file size, e.g. "10.2M" or "12
-- bytes".
prettyFileSize :: Int64
               -- ^ A file size in bytes.
               -> T.Text
prettyFileSize i
    | i >= 2 ^ (40::Int64) = T.pack $ format (i `divBy` (2 ** 40)) <> "T"
    | i >= 2 ^ (30::Int64) = T.pack $ format (i `divBy` (2 ** 30)) <> "G"
    | i >= 2 ^ (20::Int64) = T.pack $ format (i `divBy` (2 ** 20)) <> "M"
    | i >= 2 ^ (10::Int64) = T.pack $ format (i `divBy` (2 ** 10)) <> "K"
    | otherwise    = T.pack $ show i <> " bytes"
    where
        format = printf "%0.1f"
        divBy :: Int64 -> Double -> Double
        divBy a b = ((fromIntegral a) :: Double) / b

-- | Build a list of file info entries for the specified directory. This
-- does not catch any exceptions, so the caller is responsible for
-- handling them.
--
-- The entries returned are all entries in the specified directory
-- except for @.@ and @..@. Directories are always given first. Entries
-- are sorted in case-insensitive lexicographic order.
--
-- This function is exported for those who want to implement their own
-- file browser using the types in this module.
entriesForDirectory :: FilePath -> IO [FileInfo]
entriesForDirectory rawPath = do
    path <- D.makeAbsolute rawPath

    -- Get all entries except "." and "..", then sort them
    dirContents <- D.listDirectory path

    infos <- forM dirContents $ \f -> do
        filePath <- D.canonicalizePath $ path FP.</> f
        status <- U.getFileStatus filePath
        let U.COff sz = U.fileSize status
        return FileInfo { fileInfoFilename = f
                        , fileInfoFilePath = filePath
                        , fileInfoSanitizedFilename = sanitizeFilename f
                        , fileInfoFileType = fileTypeFromStatus status
                        , fileInfoFileSize = sz
                        }

    let dirsFirst a b = if fileInfoFileType a == Just Directory &&
                           fileInfoFileType b == Just Directory
                        then compare (toLower <$> fileInfoFilename a)
                                     (toLower <$> fileInfoFilename b)
                        else if fileInfoFileType a == Just Directory &&
                                fileInfoFileType b /= Just Directory
                             then LT
                             else if fileInfoFileType b == Just Directory &&
                                     fileInfoFileType a /= Just Directory
                                  then GT
                                  else compare (toLower <$> fileInfoFilename a)
                                               (toLower <$> fileInfoFilename b)

        allEntries = sortBy dirsFirst infos

    return allEntries

fileTypeFromStatus :: U.FileStatus -> Maybe FileType
fileTypeFromStatus s =
    if | U.isBlockDevice s     -> Just BlockDevice
       | U.isCharacterDevice s -> Just CharacterDevice
       | U.isNamedPipe s       -> Just NamedPipe
       | U.isRegularFile s     -> Just RegularFile
       | U.isDirectory s       -> Just Directory
       | U.isSocket s          -> Just UnixSocket
       | U.isSymbolicLink s    -> Just SymbolicLink
       | otherwise             -> Nothing

-- | Get the file information for the file under the cursor, if any.
fileBrowserCursor :: FileBrowser n -> Maybe FileInfo
fileBrowserCursor b = snd <$> listSelectedElement (b^.fileBrowserEntriesL)

-- | Handle a Vty input event.
--
-- Events handled regardless of mode:
--
-- * @Enter@: set the file browser's selected entry
--   ('fileBrowserSelection') for use by the calling application
-- * @Ctrl-n@: select the next entry
-- * @Ctrl-p@: select the previous entry
-- * 'List' navigation keys
--
-- Events handled only in normal mode:
--
-- * @/@: enter search mode
--
-- Events handled only in search mode:
--
-- * @Esc@, @Ctrl-C@: cancel search mode
-- * Text input: update search string
handleFileBrowserEvent :: (Ord n) => Vty.Event -> FileBrowser n -> EventM n (FileBrowser n)
handleFileBrowserEvent e b =
    if fileBrowserIsSearching b
    then handleFileBrowserEventSearching e b
    else handleFileBrowserEventNormal e b

safeInit :: T.Text -> T.Text
safeInit t | T.length t == 0 = t
           | otherwise = T.init t

handleFileBrowserEventSearching :: (Ord n) => Vty.Event -> FileBrowser n -> EventM n (FileBrowser n)
handleFileBrowserEventSearching e b =
    case e of
        Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] ->
            return $ updateFileBrowserSearch (const Nothing) b
        Vty.EvKey Vty.KEsc [] ->
            return $ updateFileBrowserSearch (const Nothing) b
        Vty.EvKey Vty.KBS [] ->
            return $ updateFileBrowserSearch (fmap safeInit) b
        Vty.EvKey Vty.KEnter [] ->
            maybeSelectCurrentEntry b
        Vty.EvKey (Vty.KChar c) [] ->
            return $ updateFileBrowserSearch (fmap (flip T.snoc c)) b
        _ ->
            handleFileBrowserEventCommon e b

handleFileBrowserEventNormal :: (Ord n) => Vty.Event -> FileBrowser n -> EventM n (FileBrowser n)
handleFileBrowserEventNormal e b =
    case e of
        Vty.EvKey (Vty.KChar '/') [] ->
            -- Begin file search
            return $ updateFileBrowserSearch (const $ Just "") b
        Vty.EvKey Vty.KEnter [] ->
            -- Select file or enter directory
            maybeSelectCurrentEntry b
        _ ->
            handleFileBrowserEventCommon e b

handleFileBrowserEventCommon :: (Ord n) => Vty.Event -> FileBrowser n -> EventM n (FileBrowser n)
handleFileBrowserEventCommon e b =
    case e of
        Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] ->
            return $ b & fileBrowserEntriesL %~ listMoveBy 1
        Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl] ->
            return $ b & fileBrowserEntriesL %~ listMoveBy (-1)
        _ ->
            handleEventLensed b fileBrowserEntriesL handleListEvent e

maybeSelectCurrentEntry :: FileBrowser n -> EventM n (FileBrowser n)
maybeSelectCurrentEntry b =
    case fileBrowserCursor b of
        Nothing -> return b
        Just entry ->
            case fileInfoFileType entry of
                Just Directory -> liftIO $ setWorkingDirectory (fileInfoFilePath entry) b
                _ -> return $ b & fileBrowserSelectionStateL .~ Just entry

-- | Render a file browser. This renders a list of entries in the
-- working directory, a cursor to select from among the entries, a
-- header displaying the working directory, and a footer displaying
-- information about the selected entry.
--
-- Note that if the most recent file browser operation produced an
-- exception in 'fileBrowserException', that exception is not rendered
-- by this function. That exception needs to be rendered (if at all) by
-- the calling application.
--
-- The file browser is greedy in both dimensions.
renderFileBrowser :: (Show n, Ord n)
                  => Bool
                  -- ^ Whether the file browser has input focus.
                  -> FileBrowser n
                  -- ^ The browser to render.
                  -> Widget n
renderFileBrowser foc b =
    let maxFilenameLength = maximum $ (length . fileInfoFilename) <$> (b^.fileBrowserEntriesL)
        cwdHeader = padRight Max $
                    str $ sanitizeFilename $ fileBrowserWorkingDirectory b
        selInfo = case listSelectedElement (b^.fileBrowserEntriesL) of
            Nothing -> vLimit 1 $ fill ' '
            Just (_, i) -> padRight Max $ selInfoFor i
        fileTypeLabel Nothing = "unknown"
        fileTypeLabel (Just t) =
            case t of
                RegularFile -> "file"
                BlockDevice -> "block device"
                CharacterDevice -> "character device"
                NamedPipe -> "pipe"
                Directory -> "directory"
                SymbolicLink -> "symbolic link"
                UnixSocket -> "socket"
        selInfoFor i =
            let maybeSize = if fileInfoFileType i == Just RegularFile
                            then ", " <> prettyFileSize (fileInfoFileSize i)
                            else ""
            in txt $ (T.pack $ fileInfoSanitizedFilename i) <> ": " <>
                     fileTypeLabel (fileInfoFileType i) <> maybeSize
        maybeSearchInfo = case b^.fileBrowserSearchStringL of
            Nothing -> emptyWidget
            Just s -> padRight Max $
                      txt "Search: " <+>
                      showCursor (b^.fileBrowserNameL) (Location (T.length s, 0)) (txt s)

    in withDefAttr fileBrowserAttr $
       vBox [ withDefAttr fileBrowserCurrentDirectoryAttr cwdHeader
            , renderList (renderFileInfo foc maxFilenameLength) foc (b^.fileBrowserEntriesL)
            , maybeSearchInfo
            , withDefAttr fileBrowserSelectionInfoAttr selInfo
            ]

renderFileInfo :: Bool -> Int -> Bool -> FileInfo -> Widget n
renderFileInfo foc maxLen sel info =
    (if foc
     then (if sel then forceAttr listSelectedFocusedAttr else id)
     else (if sel then forceAttr listSelectedAttr else id)) $
    padRight Max body
    where
        addAttr = maybe id (withDefAttr . attrForFileType) (fileInfoFileType info)
        body = addAttr (hLimit (maxLen + 1) $ padRight Max $ str $ fileInfoSanitizedFilename info <> suffix)
        suffix = if fileInfoFileType info == Just Directory
                 then "/"
                 else ""

-- | Sanitize a filename for terminal display, replacing non-printable
-- characters with '?'.
sanitizeFilename :: String -> String
sanitizeFilename = fmap toPrint
    where
        toPrint c | isPrint c = c
                  | otherwise = '?'

attrForFileType :: FileType -> AttrName
attrForFileType RegularFile = fileBrowserRegularFileAttr
attrForFileType BlockDevice = fileBrowserBlockDeviceAttr
attrForFileType CharacterDevice = fileBrowserCharacterDeviceAttr
attrForFileType NamedPipe = fileBrowserNamedPipeAttr
attrForFileType Directory = fileBrowserDirectoryAttr
attrForFileType SymbolicLink = fileBrowserSymbolicLinkAttr
attrForFileType UnixSocket = fileBrowserUnixSocketAttr

-- | The base attribute for all file browser attributes.
fileBrowserAttr :: AttrName
fileBrowserAttr = "fileBrowser"

-- | The attribute used for the current directory displayed at the top
-- of the browser.
fileBrowserCurrentDirectoryAttr :: AttrName
fileBrowserCurrentDirectoryAttr = fileBrowserAttr <> "currentDirectory"

-- | The attribute used for the entry information displayed at the
-- bottom of the browser.
fileBrowserSelectionInfoAttr :: AttrName
fileBrowserSelectionInfoAttr = fileBrowserAttr <> "selectionInfo"

-- | The attribute used to render directory entries.
fileBrowserDirectoryAttr :: AttrName
fileBrowserDirectoryAttr = fileBrowserAttr <> "directory"

-- | The attribute used to render block device entries.
fileBrowserBlockDeviceAttr :: AttrName
fileBrowserBlockDeviceAttr = fileBrowserAttr <> "block"

-- | The attribute used to render regular file entries.
fileBrowserRegularFileAttr :: AttrName
fileBrowserRegularFileAttr = fileBrowserAttr <> "regular"

-- | The attribute used to render character device entries.
fileBrowserCharacterDeviceAttr :: AttrName
fileBrowserCharacterDeviceAttr = fileBrowserAttr <> "char"

-- | The attribute used to render named pipe entries.
fileBrowserNamedPipeAttr :: AttrName
fileBrowserNamedPipeAttr = fileBrowserAttr <> "pipe"

-- | The attribute used to render symbolic link entries.
fileBrowserSymbolicLinkAttr :: AttrName
fileBrowserSymbolicLinkAttr = fileBrowserAttr <> "symlink"

-- | The attribute used to render Unix socket entries.
fileBrowserUnixSocketAttr :: AttrName
fileBrowserUnixSocketAttr = fileBrowserAttr <> "unixSocket"

-- | A file type filter for use with 'setFileBrowserEntryFilter'. This
-- filter permits entries whose file types are in the specified list.
fileTypeMatch :: [FileType] -> FileInfo -> Bool
fileTypeMatch tys i = maybe False (`elem` tys) $ fileInfoFileType i

-- | A filter that matches any directory regardless of name, or any
-- regular file with the specified extension. For example, an extension
-- argument of @"xml"@ would match regular files @test.xml@ and
-- @TEST.XML@ and it will match directories regardless of name.
fileExtensionMatch :: String -> FileInfo -> Bool
fileExtensionMatch ext i =
    ('.' : (toLower <$> ext)) `isSuffixOf` (toLower <$> fileInfoFilename i)
