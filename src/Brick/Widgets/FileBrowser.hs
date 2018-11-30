{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskell #-}
module Brick.Widgets.FileBrowser
  ( FileBrowser
  , FileInfo(..)
  , FileType(..)
  , newFileBrowser
  , setCurrentDirectory
  , renderFileBrowser
  , fileBrowserSelection
  , handleFileBrowserEvent

  -- * Lenses
  , fileBrowserWorkingDirectoryL
  , fileInfoFilenameL
  , fileInfoFilePathL
  , fileInfoFileTypeL
  )
where

import Control.Monad (forM)
import Data.Char (toLower)
#if !(MIN_VERSION_base(4,11,0))
import Data.Monoid
#endif
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.Vector as V
import Lens.Micro
import qualified Graphics.Vty as Vty
import qualified System.Directory as D
import qualified System.Posix.Files as U
import qualified System.FilePath as FP

import Brick.Types
import Brick.Widgets.Core
import Brick.Widgets.List

data FileBrowser n =
    FileBrowser { fileBrowserWorkingDirectory :: FilePath
                , fileBrowserEntries :: List n FileInfo
                , fileBrowserName :: n
                }

data FileInfo =
    FileInfo { fileInfoFilename :: String
             , fileInfoFilePath :: FilePath
             , fileInfoFileType :: Maybe FileType
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
                        }

    setCurrentDirectory initialCwd b

setCurrentDirectory :: FilePath -> FileBrowser n -> IO (FileBrowser n)
setCurrentDirectory path b = do
    entries <- entriesForDirectory path
    return b { fileBrowserWorkingDirectory = path
             , fileBrowserEntries = list (b^.fileBrowserNameL) (V.fromList entries) 1
             }

entriesForDirectory :: FilePath -> IO [FileInfo]
entriesForDirectory rawPath = do
    path <- D.makeAbsolute rawPath

    -- Get all entries except "." and "..", then sort them
    sortedFiles <- sortBy (comparing (fmap toLower)) <$> D.listDirectory path

    let allFiles = "." : addParent sortedFiles
        addParent = if path == "/"
                    then id
                    else (".." :)

    forM allFiles $ \f -> do
        filePath <- D.makeAbsolute $ path FP.</> f
        status <- U.getFileStatus filePath
        return FileInfo { fileInfoFilename = f
                        , fileInfoFilePath = filePath
                        , fileInfoFileType = fileTypeFromStatus status
                        }

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

fileBrowserSelection :: FileBrowser n -> Maybe FileInfo
fileBrowserSelection b = snd <$> listSelectedElement (b^.fileBrowserEntriesL)

handleFileBrowserEvent :: (Ord n) => Vty.Event -> FileBrowser n -> EventM n (FileBrowser n)
handleFileBrowserEvent e b =
    handleEventLensed b fileBrowserEntriesL handleListEvent e

renderFileBrowser :: (Show n, Ord n) => Bool -> FileBrowser n -> Widget n
renderFileBrowser foc b =
    (str $ fileBrowserWorkingDirectory b) <=>
    renderList renderFileInfo foc (b^.fileBrowserEntriesL)

renderFileInfo :: Bool -> FileInfo -> Widget n
renderFileInfo _ info =
    padRight Max body
    where
        body = str $ fileInfoFilename info <> " (" <> (show $ fileInfoFileType info) <> ")"
