{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module provides a file browser widget that allows users to
-- navigate directory trees, search for files and directories, and
-- select entries of interest. For a complete working demonstration of
-- this module, see @programs/FileBrowserDemo.hs@.
--
-- To use this module:
--
-- * Embed a 'FileBrowser' in your application state.
-- * Dispatch events to it in your event handler with
--   'handleFileBrowserEvent'.
-- * Get the entry under the browser's cursor with 'fileBrowserCursor'
--   and get the entries selected by the user with 'Enter' or 'Space'
--   using 'fileBrowserSelection'.
-- * Inspect 'fileBrowserException' to determine whether the
--   file browser encountered an error when reading a directory in
--   'setWorkingDirectory' or when changing directories in the event
--   handler.
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
  , FileStatus(..)
  , FileType(..)

  -- * Making a new file browser
  , newFileBrowser
  , selectNonDirectories
  , selectDirectories

  -- * Manipulating a file browser's state
  , setWorkingDirectory
  , getWorkingDirectory
  , updateFileBrowserSearch
  , setFileBrowserEntryFilter

  -- * Actions
  , actionFileBrowserBeginSearch
  , actionFileBrowserSelectEnter
  , actionFileBrowserSelectCurrent
  , actionFileBrowserListPageUp
  , actionFileBrowserListPageDown
  , actionFileBrowserListHalfPageUp
  , actionFileBrowserListHalfPageDown
  , actionFileBrowserListTop
  , actionFileBrowserListBottom
  , actionFileBrowserListNext
  , actionFileBrowserListPrev

  -- * Handling events
  , handleFileBrowserEvent
  , maybeSelectCurrentEntry

  -- * Rendering
  , renderFileBrowser

  -- * Getting information
  , fileBrowserCursor
  , fileBrowserIsSearching
  , fileBrowserSelection
  , fileBrowserException
  , fileBrowserSelectable
  , fileInfoFileType

  -- * Attributes
  , fileBrowserAttr
  , fileBrowserCurrentDirectoryAttr
  , fileBrowserSelectionInfoAttr
  , fileBrowserSelectedAttr
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
  , fileBrowserSelectableL
  , fileInfoFilenameL
  , fileInfoSanitizedFilenameL
  , fileInfoFilePathL
  , fileInfoFileStatusL
  , fileInfoLinkTargetTypeL
  , fileStatusSizeL
  , fileStatusFileTypeL

  -- * Getters
  , fileBrowserEntryFilterG
  , fileBrowserWorkingDirectoryG
  , fileBrowserEntriesG
  , fileBrowserLatestResultsG
  , fileBrowserSelectedFilesG
  , fileBrowserNameG
  , fileBrowserSearchStringG
  , fileBrowserExceptionG
  , fileBrowserSelectableG

  -- * Miscellaneous
  , prettyFileSize

  -- * Utilities
  , entriesForDirectory
  , getFileInfo
  )
where

import qualified Control.Exception as E
import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower, isPrint)
import Data.Foldable (for_)
import Data.Maybe (fromMaybe, isJust, fromJust)
import qualified Data.Foldable as F
import qualified Data.Text as T
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Int (Int64)
import Data.List (sortBy, isSuffixOf, dropWhileEnd)
import qualified Data.Set as Set
import qualified Data.Vector as V
import Lens.Micro
import Lens.Micro.Mtl ((%=))
import Lens.Micro.TH (lensRules, generateUpdateableOptics)
import qualified Graphics.Vty as Vty
import qualified System.Directory as D
import qualified System.PosixCompat.Files as U
import qualified System.PosixCompat.Types as U
import qualified System.FilePath as FP
import Text.Printf (printf)

import Brick.Types
import Brick.AttrMap (AttrName, attrName)
import Brick.Widgets.Core
import Brick.Widgets.List

-- | A file browser's state. Embed this in your application state and
-- transform it with 'handleFileBrowserEvent' and the functions included
-- in this module.
data FileBrowser n =
    FileBrowser { fileBrowserWorkingDirectory :: FilePath
                , fileBrowserEntries :: List n FileInfo
                , fileBrowserLatestResults :: [FileInfo]
                , fileBrowserSelectedFiles :: Set.Set String
                , fileBrowserName :: n
                , fileBrowserEntryFilter :: Maybe (FileInfo -> Bool)
                , fileBrowserSearchString :: Maybe T.Text
                , fileBrowserException :: Maybe E.IOException
                -- ^ The exception status of the latest directory
                -- change. If 'Nothing', the latest directory change
                -- was successful and all entries were read. Otherwise,
                -- this contains the exception raised by the latest
                -- directory change in case the calling application
                -- needs to inspect or present the error to the user.
                , fileBrowserSelectable :: FileInfo -> Bool
                -- ^ The function that determines what kinds of entries
                -- are selectable with in the event handler. Note that
                -- if this returns 'True' for an entry, an @Enter@ or
                -- @Space@ keypress selects that entry rather than doing
                -- anything else; directory changes can only occur if
                -- this returns 'False' for directories.
                --
                -- Note that this is a record field so it can be used to
                -- change the selection function.
                }

instance Named (FileBrowser n) n where
    getName = getName . fileBrowserEntries

-- | File status information.
data FileStatus =
    FileStatus { fileStatusSize :: Int64
               -- ^ The size, in bytes, of this entry's file.
               , fileStatusFileType :: Maybe FileType
               -- ^ The type of this entry's file, if it could be
               -- determined.
               }
               deriving (Show, Eq)

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
             , fileInfoFileStatus :: Either E.IOException FileStatus
             -- ^ The file status if it could be obtained, or the
             -- exception that was caught when attempting to read the
             -- file's status.
             , fileInfoLinkTargetType :: Maybe FileType
             -- ^ If this entry is a symlink, this indicates the type of
             -- file the symlink points to, if it could be obtained.
             }
             deriving (Show, Eq)

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
suffixLensesWith "G" (lensRules & generateUpdateableOptics .~ False) ''FileBrowser
suffixLenses ''FileInfo
suffixLenses ''FileStatus

-- | Make a new file browser state. The provided resource name will be
-- used to render the 'List' viewport of the browser.
--
-- By default, the browser will show all files and directories
-- in its working directory. To change that behavior, see
-- 'setFileBrowserEntryFilter'.
newFileBrowser :: (FileInfo -> Bool)
               -- ^ The function used to determine what kinds of entries
               -- can be selected (see 'handleFileBrowserEvent'). A
               -- good default is 'selectNonDirectories'. This can be
               -- changed at 'any time with 'fileBrowserSelectable' or
               -- its 'corresponding lens.
               -> n
               -- ^ The resource name associated with the browser's
               -- entry listing.
               -> Maybe FilePath
               -- ^ The initial working directory that the browser
               -- displays. If not provided, this defaults to the
               -- executable's current working directory.
               -> IO (FileBrowser n)
newFileBrowser selPredicate name mCwd = do
    initialCwd <- FP.normalise <$> case mCwd of
        Just path -> return $ removeTrailingSlash path
        Nothing -> D.getCurrentDirectory

    let b = FileBrowser { fileBrowserWorkingDirectory = initialCwd
                        , fileBrowserEntries = list name mempty 1
                        , fileBrowserLatestResults = mempty
                        , fileBrowserSelectedFiles = mempty
                        , fileBrowserName = name
                        , fileBrowserEntryFilter = Nothing
                        , fileBrowserSearchString = Nothing
                        , fileBrowserException = Nothing
                        , fileBrowserSelectable = selPredicate
                        }

    setWorkingDirectory initialCwd b

-- | Removes any trailing slash(es) from the supplied FilePath (which should
-- indicate a directory).  This does not remove a sole slash indicating the root
-- directory.
--
-- This is done because if the FileBrowser is initialized with an initial working
-- directory that ends in a slash, then selecting the "../" entry to move to the
-- parent directory will cause the removal of the trailing slash, but it will not
-- otherwise cause any change, misleading the user into thinking no action was
-- taken (the disappearance of the trailing slash is unlikely to be noticed).
-- All subsequent parent directory selection operations are processed normally,
-- and the 'fileBrowserWorkingDirectory' never ends in a trailing slash
-- thereafter (except at the root directory).
removeTrailingSlash :: FilePath -> FilePath
removeTrailingSlash "/" = "/"
removeTrailingSlash d = dropWhileEnd (== '/') d

-- | A file entry selector that permits selection of all file entries
-- except directories. Use this if you want users to be able to navigate
-- directories in the browser. If you want users to be able to select
-- only directories, use 'selectDirectories'.
selectNonDirectories :: FileInfo -> Bool
selectNonDirectories i =
    case fileInfoFileType i of
        Just Directory -> False
        Just SymbolicLink ->
            case fileInfoLinkTargetType i of
                Just Directory -> False
                _ -> True
        _ -> True

-- | A file entry selector that permits selection of directories
-- only. This prevents directory navigation and only supports directory
-- selection.
selectDirectories :: FileInfo -> Bool
selectDirectories i =
    case fileInfoFileType i of
        Just Directory -> True
        Just SymbolicLink -> fileInfoLinkTargetType i == Just Directory
        _ -> False

-- | Set the filtering function used to determine which entries in
-- the browser's current directory appear in the browser. 'Nothing'
-- indicates no filtering, meaning all entries will be shown. 'Just'
-- indicates a function that should return 'True' for entries that
-- should be permitted to appear.
--
-- Note that this applies the filter after setting it by updating the
-- listed entries to reflect the result of the filter. That is unlike
-- setting the filter with the 'fileBrowserEntryFilterL' lens directly,
-- which just sets the filter but does not (and cannot) update the
-- listed entries.
setFileBrowserEntryFilter :: Maybe (FileInfo -> Bool) -> FileBrowser n -> FileBrowser n
setFileBrowserEntryFilter f b =
    applyFilterAndSearch $ b & fileBrowserEntryFilterL .~ f

-- | Set the working directory of the file browser. This scans the new
-- directory and repopulates the browser while maintaining any active
-- search string and/or entry filtering.
--
-- If the directory scan raises an 'IOException', the exception is
-- stored in the browser and is accessible with 'fileBrowserException'. If
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

    return $ setEntries allEntries b
                 & fileBrowserWorkingDirectoryL .~ path
                 & fileBrowserExceptionL .~ exc
                 & fileBrowserSelectedFilesL .~ mempty

parentOf :: FilePath -> IO FileInfo
parentOf path = getFileInfo ".." $ FP.takeDirectory path

-- | Build a 'FileInfo' for the specified file and path. If an
-- 'IOException' is raised while attempting to get the file information,
-- the 'fileInfoFileStatus' field is populated with the exception.
-- Otherwise it is populated with the 'FileStatus' for the file.
getFileInfo :: String
            -- ^ The name of the file to inspect. This filename is only
            -- used to set the 'fileInfoFilename' and sanitized filename
            -- fields; the actual file to be inspected is referred
            -- to by the second argument. This is decomposed so that
            -- 'FileInfo's can be used to represent information about
            -- entries like @..@, whose display names differ from their
            -- physical paths.
            -> FilePath
            -- ^ The actual full path to the file or directory to
            -- inspect.
            -> IO FileInfo
getFileInfo name = go []
    where
        go history fullPath = do
            filePath <- D.makeAbsolute fullPath
            statusResult <- E.try $ U.getSymbolicLinkStatus filePath

            let stat = do
                  status <- statusResult
                  let U.COff sz = U.fileSize status
                  return FileStatus { fileStatusFileType = fileTypeFromStatus status
                                    , fileStatusSize = sz
                                    }

            targetTy <- case fileStatusFileType <$> stat of
                Right (Just SymbolicLink) -> do
                    targetPathResult <- E.try $ U.readSymbolicLink filePath
                    case targetPathResult of
                        Left (_::E.SomeException) -> return Nothing
                        Right targetPath ->
                            -- Watch out for recursive symlink chains:
                            -- if history starts repeating, abort the
                            -- symlink following process.
                            --
                            -- Examples:
                            --   $ ln -s foo foo
                            --
                            --   $ ln -s foo bar
                            --   $ ln -s bar foo
                            if targetPath `elem` history
                            then return Nothing
                            else do
                                targetInfo <- liftIO $ go (fullPath : history) targetPath
                                case fileInfoFileStatus targetInfo of
                                    Right (FileStatus _ targetTy) -> return targetTy
                                    _ -> return Nothing
                _ -> return Nothing

            return FileInfo { fileInfoFilename = name
                            , fileInfoFilePath = filePath
                            , fileInfoSanitizedFilename = sanitizeFilename name
                            , fileInfoFileStatus = stat
                            , fileInfoLinkTargetType = targetTy
                            }

-- | Get the file type for this file info entry. If the file type could
-- not be obtained due to an 'IOException', return 'Nothing'.
fileInfoFileType :: FileInfo -> Maybe FileType
fileInfoFileType i =
    case fileInfoFileStatus i of
        Left _ -> Nothing
        Right stat -> fileStatusFileType stat

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

-- | Get the entries chosen by the user, if any. Entries are chosen by
-- an 'Enter' or 'Space' keypress; if you want the entry under the
-- cursor, use 'fileBrowserCursor'.
fileBrowserSelection :: FileBrowser n -> [FileInfo]
fileBrowserSelection b =
    let getEntry filename = fromJust $ F.find ((== filename) . fileInfoFilename) $ b^.fileBrowserLatestResultsL
    in fmap getEntry $ F.toList $ b^.fileBrowserSelectedFilesL

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
                            (\search i -> T.toLower search `T.isInfixOf` T.pack (toLower <$> fileInfoSanitizedFilename i))
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
-- function does not catch any exceptions raised by calling
-- 'makeAbsolute' or 'listDirectory', but it does catch exceptions on
-- a per-file basis. Any exceptions caught when inspecting individual
-- files are stored in the 'fileInfoFileStatus' field of each
-- 'FileInfo'.
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
        getFileInfo f (path FP.</> f)

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

-- | Handle a Vty input event. Note that event handling can
-- cause a directory change so the caller should be aware that
-- 'fileBrowserException' may need to be checked after handling an
-- event in case an exception was triggered while scanning the working
-- directory.
--
-- Events handled regardless of mode:
--
-- * @Ctrl-b@: 'actionFileBrowserListPageUp'
-- * @Ctrl-f@: 'actionFileBrowserListPageDown'
-- * @Ctrl-d@: 'actionFileBrowserListHalfPageDown'
-- * @Ctrl-u@: 'actionFileBrowserListHalfPageUp'
-- * @Ctrl-n@: 'actionFileBrowserListNext'
-- * @Ctrl-p@: 'actionFileBrowserListPrev'
--
-- Events handled only in normal mode:
--
-- * @/@: 'actionFileBrowserBeginSearch'
-- * @Enter@: 'actionFileBrowserSelectEnter'
-- * @Space@: 'actionFileBrowserSelectCurrent'
-- * @g@: 'actionFileBrowserListTop'
-- * @G@: 'actionFileBrowserListBottom'
-- * @j@: 'actionFileBrowserListNext'
-- * @k@: 'actionFileBrowserListPrev'
--
-- Events handled only in search mode:
--
-- * @Esc@, @Ctrl-C@: cancel search mode
-- * Text input: update search string

actionFileBrowserBeginSearch :: EventM n (FileBrowser n) ()
actionFileBrowserBeginSearch =
    modify $ updateFileBrowserSearch (const $ Just "")

actionFileBrowserSelectEnter :: EventM n (FileBrowser n) ()
actionFileBrowserSelectEnter =
    maybeSelectCurrentEntry

actionFileBrowserSelectCurrent :: EventM n (FileBrowser n) ()
actionFileBrowserSelectCurrent =
    selectCurrentEntry

actionFileBrowserListPageUp :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListPageUp =
    zoom fileBrowserEntriesL listMovePageUp

actionFileBrowserListPageDown :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListPageDown =
    zoom fileBrowserEntriesL listMovePageDown

actionFileBrowserListHalfPageUp :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListHalfPageUp =
    zoom fileBrowserEntriesL (listMoveByPages (-0.5::Double))

actionFileBrowserListHalfPageDown :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListHalfPageDown =
    zoom fileBrowserEntriesL (listMoveByPages (0.5::Double))

actionFileBrowserListTop :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListTop =
    fileBrowserEntriesL %= listMoveTo 0

actionFileBrowserListBottom :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListBottom = do
    b <- get
    let sz = length (listElements $ b^.fileBrowserEntriesL)
    fileBrowserEntriesL %= listMoveTo (sz - 1)

actionFileBrowserListNext :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListNext =
    fileBrowserEntriesL %= listMoveBy 1

actionFileBrowserListPrev :: Ord n => EventM n (FileBrowser n) ()
actionFileBrowserListPrev =
    fileBrowserEntriesL %= listMoveBy (-1)

handleFileBrowserEvent :: (Ord n) => Vty.Event -> EventM n (FileBrowser n) ()
handleFileBrowserEvent e = do
    b <- get
    if fileBrowserIsSearching b
        then handleFileBrowserEventSearching e
        else handleFileBrowserEventNormal e

safeInit :: T.Text -> T.Text
safeInit t | T.length t == 0 = t
           | otherwise = T.init t

handleFileBrowserEventSearching :: (Ord n) => Vty.Event -> EventM n (FileBrowser n) ()
handleFileBrowserEventSearching e =
    case e of
        Vty.EvKey (Vty.KChar 'c') [Vty.MCtrl] ->
            modify $ updateFileBrowserSearch (const Nothing)
        Vty.EvKey Vty.KEsc [] ->
            modify $ updateFileBrowserSearch (const Nothing)
        Vty.EvKey Vty.KBS [] ->
            modify $ updateFileBrowserSearch (fmap safeInit)
        Vty.EvKey Vty.KEnter [] -> do
            maybeSelectCurrentEntry
            modify $ updateFileBrowserSearch (const Nothing)
        Vty.EvKey (Vty.KChar c) [] ->
            modify $ updateFileBrowserSearch (fmap (flip T.snoc c))
        _ ->
            handleFileBrowserEventCommon e

handleFileBrowserEventNormal :: (Ord n) => Vty.Event -> EventM n (FileBrowser n) ()
handleFileBrowserEventNormal e =
    case e of
        Vty.EvKey (Vty.KChar '/') [] ->
            -- Begin file search
            actionFileBrowserBeginSearch
        Vty.EvKey Vty.KEnter [] ->
            -- Select file or enter directory
            actionFileBrowserSelectEnter
        Vty.EvKey (Vty.KChar ' ') [] ->
            -- Select entry
            actionFileBrowserSelectCurrent
        _ ->
            handleFileBrowserEventCommon e

handleFileBrowserEventCommon :: (Ord n) => Vty.Event -> EventM n (FileBrowser n) ()
handleFileBrowserEventCommon e =
    case e of
        Vty.EvKey (Vty.KChar 'b') [Vty.MCtrl] ->
            actionFileBrowserListPageUp
        Vty.EvKey (Vty.KChar 'f') [Vty.MCtrl] ->
            actionFileBrowserListPageDown
        Vty.EvKey (Vty.KChar 'd') [Vty.MCtrl] ->
            actionFileBrowserListHalfPageDown
        Vty.EvKey (Vty.KChar 'u') [Vty.MCtrl] ->
            actionFileBrowserListHalfPageUp
        Vty.EvKey (Vty.KChar 'g') [] ->
            actionFileBrowserListTop
        Vty.EvKey (Vty.KChar 'G') [] ->
            actionFileBrowserListBottom
        Vty.EvKey (Vty.KChar 'j') [] ->
            actionFileBrowserListNext
        Vty.EvKey (Vty.KChar 'k') [] ->
            actionFileBrowserListPrev
        Vty.EvKey (Vty.KChar 'n') [Vty.MCtrl] ->
            actionFileBrowserListNext
        Vty.EvKey (Vty.KChar 'p') [Vty.MCtrl] ->
            actionFileBrowserListPrev
        _ ->
            zoom fileBrowserEntriesL $ handleListEvent e

markSelected :: FileInfo -> EventM n (FileBrowser n) ()
markSelected e = fileBrowserSelectedFilesL %= Set.insert (fileInfoFilename e)

-- | If the browser's current entry is selectable according to
-- @fileBrowserSelectable@, add it to the selection set and return.
-- If not, and if the entry is a directory or a symlink targeting a
-- directory, set the browser's current path to the selected directory.
--
-- Otherwise, return the browser state unchanged.
maybeSelectCurrentEntry :: EventM n (FileBrowser n) ()
maybeSelectCurrentEntry = do
    b <- get
    for_ (fileBrowserCursor b) $ \entry ->
        if fileBrowserSelectable b entry
        then markSelected entry
        else when (selectDirectories entry) $
            put =<< liftIO (setWorkingDirectory (fileInfoFilePath entry) b)

selectCurrentEntry :: EventM n (FileBrowser n) ()
selectCurrentEntry = do
    b <- get
    for_ (fileBrowserCursor b) markSelected

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
    let maxFilenameLength = maximum $ length . fileInfoFilename <$> (b^.fileBrowserEntriesL)
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
            let label = case fileInfoFileStatus i of
                    Left _ -> "unknown"
                    Right stat ->
                        let maybeSize = if fileStatusFileType stat == Just RegularFile
                                        then ", " <> prettyFileSize (fileStatusSize stat)
                                        else ""
                        in fileTypeLabel (fileStatusFileType stat) <> maybeSize
            in txt $ T.pack (fileInfoSanitizedFilename i) <> ": " <> label

        maybeSearchInfo = case b^.fileBrowserSearchStringL of
            Nothing -> emptyWidget
            Just s -> padRight Max $
                      txt "Search: " <+>
                      showCursor (b^.fileBrowserNameL) (Location (T.length s, 0)) (txt s)

    in withDefAttr fileBrowserAttr $
       vBox [ withDefAttr fileBrowserCurrentDirectoryAttr cwdHeader
            , renderList (renderFileInfo foc maxFilenameLength (b^.fileBrowserSelectedFilesL) (b^.fileBrowserNameL))
                         foc (b^.fileBrowserEntriesL)
            , maybeSearchInfo
            , withDefAttr fileBrowserSelectionInfoAttr selInfo
            ]

renderFileInfo :: Bool -> Int -> Set.Set String -> n -> Bool -> FileInfo -> Widget n
renderFileInfo foc maxLen selFiles n listSel info =
    (if foc
     then (if listSel then forceAttr listSelectedFocusedAttr
               else if sel then forceAttr fileBrowserSelectedAttr else id)
     else (if listSel then forceAttr listSelectedAttr
               else if sel then forceAttr fileBrowserSelectedAttr else id)) $
    padRight Max body
    where
        sel = fileInfoFilename info `Set.member` selFiles
        addAttr = maybe id (withDefAttr . attrForFileType) (fileInfoFileType info)
        body = addAttr (hLimit (maxLen + 1) $
               padRight Max $
               (if foc && listSel then putCursor n (Location (0,0)) else id) $
               str $ fileInfoSanitizedFilename info <> suffix)
        suffix = (if fileInfoFileType info == Just Directory then "/" else "") <>
                 (if sel then "*" else "")

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
fileBrowserAttr = attrName "fileBrowser"

-- | The attribute used for the current directory displayed at the top
-- of the browser.
fileBrowserCurrentDirectoryAttr :: AttrName
fileBrowserCurrentDirectoryAttr = fileBrowserAttr <> attrName "currentDirectory"

-- | The attribute used for the entry information displayed at the
-- bottom of the browser.
fileBrowserSelectionInfoAttr :: AttrName
fileBrowserSelectionInfoAttr = fileBrowserAttr <> attrName "selectionInfo"

-- | The attribute used to render directory entries.
fileBrowserDirectoryAttr :: AttrName
fileBrowserDirectoryAttr = fileBrowserAttr <> attrName "directory"

-- | The attribute used to render block device entries.
fileBrowserBlockDeviceAttr :: AttrName
fileBrowserBlockDeviceAttr = fileBrowserAttr <> attrName "block"

-- | The attribute used to render regular file entries.
fileBrowserRegularFileAttr :: AttrName
fileBrowserRegularFileAttr = fileBrowserAttr <> attrName "regular"

-- | The attribute used to render character device entries.
fileBrowserCharacterDeviceAttr :: AttrName
fileBrowserCharacterDeviceAttr = fileBrowserAttr <> attrName "char"

-- | The attribute used to render named pipe entries.
fileBrowserNamedPipeAttr :: AttrName
fileBrowserNamedPipeAttr = fileBrowserAttr <> attrName "pipe"

-- | The attribute used to render symbolic link entries.
fileBrowserSymbolicLinkAttr :: AttrName
fileBrowserSymbolicLinkAttr = fileBrowserAttr <> attrName "symlink"

-- | The attribute used to render Unix socket entries.
fileBrowserUnixSocketAttr :: AttrName
fileBrowserUnixSocketAttr = fileBrowserAttr <> attrName "unixSocket"

-- | The attribute used for selected entries in the file browser.
fileBrowserSelectedAttr :: AttrName
fileBrowserSelectedAttr = fileBrowserAttr <> attrName "selected"

-- | A file type filter for use with 'setFileBrowserEntryFilter'. This
-- filter permits entries whose file types are in the specified list.
fileTypeMatch :: [FileType] -> FileInfo -> Bool
fileTypeMatch tys i = maybe False (`elem` tys) $ fileInfoFileType i

-- | A filter that matches any directory regardless of name, or any
-- regular file with the specified extension. For example, an extension
-- argument of @"xml"@ would match regular files @test.xml@ and
-- @TEST.XML@ and it will match directories regardless of name.
--
-- This matcher also matches symlinks if and only if their targets are
-- directories. This is intended to make it possible to use this matcher
-- to find files with certain extensions, but also support directory
-- traversal via symlinks.
fileExtensionMatch :: String -> FileInfo -> Bool
fileExtensionMatch ext i = case fileInfoFileType i of
    Just Directory -> True
    Just RegularFile -> ('.' : (toLower <$> ext)) `isSuffixOf` (toLower <$> fileInfoFilename i)
    Just SymbolicLink -> case fileInfoLinkTargetType i of
        Just Directory -> True
        _ -> False
    _ -> False
