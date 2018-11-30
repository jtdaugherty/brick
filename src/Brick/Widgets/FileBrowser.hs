{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Brick.Widgets.FileBrowser
  ( FileBrowser(fileBrowserSelection, fileBrowserWorkingDirectory, fileBrowserEntryFilter)
  , FileInfo(..)
  , FileType(..)
  , newFileBrowser
  , setCurrentDirectory
  , renderFileBrowser
  , fileBrowserCursor
  , handleFileBrowserEvent

  -- * Lenses
  , fileBrowserWorkingDirectoryL
  , fileBrowserSelectionL
  , fileBrowserEntryFilterL
  , fileInfoFilenameL
  , fileInfoFileSizeL
  , fileInfoSanitizedFilenameL
  , fileInfoFilePathL
  , fileInfoFileTypeL

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
  , fileBrowserSocketAttr

  -- * Filters
  , fileTypeMatch
  )
where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Char (toLower, isPrint)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.Int (Int64)
import Data.List (sortBy)
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

data FileBrowser n =
    FileBrowser { fileBrowserWorkingDirectory :: FilePath
                , fileBrowserEntries :: List n FileInfo
                , fileBrowserName :: n
                , fileBrowserSelection :: Maybe FileInfo
                , fileBrowserEntryFilter :: Maybe (FileInfo -> Bool)
                }

data FileInfo =
    FileInfo { fileInfoFilename :: String
             , fileInfoSanitizedFilename :: String
             , fileInfoFilePath :: FilePath
             , fileInfoFileType :: Maybe FileType
             , fileInfoFileSize :: Int64
             }
             deriving (Show, Eq, Read)

data FileType =
    RegularFile
    | BlockDevice
    | CharacterDevice
    | NamedPipe
    | Directory
    | SymbolicLink
    | Socket
    deriving (Read, Show, Eq)

suffixLenses ''FileBrowser
suffixLenses ''FileInfo

newFileBrowser :: n -> Maybe FilePath -> IO (FileBrowser n)
newFileBrowser name mCwd = do
    initialCwd <- case mCwd of
        Just path -> return path
        Nothing -> D.getCurrentDirectory

    let b = FileBrowser { fileBrowserWorkingDirectory = initialCwd
                        , fileBrowserEntries = list name mempty 1
                        , fileBrowserName = name
                        , fileBrowserSelection = Nothing
                        , fileBrowserEntryFilter = Nothing
                        }

    setCurrentDirectory initialCwd b

setCurrentDirectory :: FilePath -> FileBrowser n -> IO (FileBrowser n)
setCurrentDirectory path b = do
    let match = fromMaybe (const True) (b^.fileBrowserEntryFilterL)
    entries <- filter match <$> entriesForDirectory path
    return b { fileBrowserWorkingDirectory = path
             , fileBrowserEntries = list (b^.fileBrowserNameL) (V.fromList entries) 1
             }

prettyFileSize :: Int64 -> T.Text
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

entriesForDirectory :: FilePath -> IO [FileInfo]
entriesForDirectory rawPath = do
    path <- D.makeAbsolute rawPath

    -- Get all entries except "." and "..", then sort them
    dirContents <- D.listDirectory path

    let addParent = if path == "/"
                    then id
                    else (parentDir :)
        parentDir = ".."
        allFiles = addParent dirContents

    infos <- forM allFiles $ \f -> do
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
       | U.isSocket s          -> Just Socket
       | U.isSymbolicLink s    -> Just SymbolicLink
       | otherwise             -> Nothing

fileBrowserCursor :: FileBrowser n -> Maybe FileInfo
fileBrowserCursor b = snd <$> listSelectedElement (b^.fileBrowserEntriesL)

handleFileBrowserEvent :: (Ord n) => Vty.Event -> FileBrowser n -> EventM n (FileBrowser n)
handleFileBrowserEvent e b =
    case e of
        Vty.EvKey Vty.KEnter [] ->
            case fileBrowserCursor b of
                Nothing -> return b
                Just entry ->
                    case fileInfoFileType entry of
                        Just Directory -> liftIO $ setCurrentDirectory (fileInfoFilePath entry) b
                        _ -> return $ b & fileBrowserSelectionL .~ Just entry
        _ ->
            handleEventLensed b fileBrowserEntriesL handleListEvent e

renderFileBrowser :: (Show n, Ord n) => Bool -> FileBrowser n -> Widget n
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
                Socket -> "socket"
        selInfoFor i =
            let maybeSize = if fileInfoFileType i == Just RegularFile
                            then ", " <> prettyFileSize (fileInfoFileSize i)
                            else ""
            in txt $ (T.pack $ fileInfoSanitizedFilename i) <> ": " <>
                     fileTypeLabel (fileInfoFileType i) <> maybeSize
    in withDefAttr fileBrowserAttr $
       vBox [ withDefAttr fileBrowserCurrentDirectoryAttr cwdHeader
            , renderList (renderFileInfo foc maxFilenameLength) foc (b^.fileBrowserEntriesL)
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
attrForFileType Socket = fileBrowserSocketAttr

fileBrowserAttr :: AttrName
fileBrowserAttr = "fileBrowser"

fileBrowserCurrentDirectoryAttr :: AttrName
fileBrowserCurrentDirectoryAttr = fileBrowserAttr <> "currentDirectory"

fileBrowserSelectionInfoAttr :: AttrName
fileBrowserSelectionInfoAttr = fileBrowserAttr <> "selectionInfo"

fileBrowserDirectoryAttr :: AttrName
fileBrowserDirectoryAttr = fileBrowserAttr <> "directory"

fileBrowserBlockDeviceAttr :: AttrName
fileBrowserBlockDeviceAttr = fileBrowserAttr <> "block"

fileBrowserRegularFileAttr :: AttrName
fileBrowserRegularFileAttr = fileBrowserAttr <> "regular"

fileBrowserCharacterDeviceAttr :: AttrName
fileBrowserCharacterDeviceAttr = fileBrowserAttr <> "char"

fileBrowserNamedPipeAttr :: AttrName
fileBrowserNamedPipeAttr = fileBrowserAttr <> "pipe"

fileBrowserSymbolicLinkAttr :: AttrName
fileBrowserSymbolicLinkAttr = fileBrowserAttr <> "symlink"

fileBrowserSocketAttr :: AttrName
fileBrowserSocketAttr = fileBrowserAttr <> "socket"

fileTypeMatch :: [FileType] -> FileInfo -> Bool
fileTypeMatch tys i = maybe False (`elem` tys) $ fileInfoFileType i
