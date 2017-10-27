{-# LANGUAGE CPP, TupleSections #-}
{-# OPTIONS -Wall #-}

module Find where

import Control.Monad (when)
import Data.ByteString.Lazy as BS (ByteString, readFile)
#if 0
import Data.Digest.Pure.SHA
#else
import Data.Digest.Pure.MD5
#endif
import Data.Map.Strict as Map (filter, fromListWith, Map)
import Data.Maybe (mapMaybe)
import Data.Set as Set (Set, singleton, size, union)
import System.Directory (getDirectoryContents)
import System.Posix.Files (getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink, modificationTime)
import System.FilePath ((</>))
import System.Posix.Types (EpochTime)

data FileType = Regular | Folder | Link | Other deriving (Show, Eq, Ord)

#if 0
type Sum = Digest SHA256State
checksum :: ByteString -> Sum
checksum = sha256
#else
type Sum = MD5Digest
checksum :: ByteString -> Sum
checksum = md5
#endif

getSubdirectoryFilesRecursive :: Bool -> FilePath -> IO [(FilePath, FileType)]
getSubdirectoryFilesRecursive verbose top = do
  (typ, subfiles1) <- getSubdirectoryFiles verbose top
  subfiles <- concat <$> mapM (getSubdirectoryFilesRecursive verbose) subfiles1
  return $ (top, typ) : subfiles

getSubdirectoryFiles :: Bool -> FilePath -> IO (FileType, [FilePath])
getSubdirectoryFiles verbose top = do
  typ <- getStatus verbose top
  case typ of
    Folder -> ((typ,) . fmap (top </>) . Prelude.filter (`notElem` [".", ".."])) <$> getDirectoryContents top
    _ -> return (typ, [])

withStatus :: Bool -> FilePath -> IO (FilePath, FileType)
withStatus verbose path = (,) <$> pure path <*> getStatus verbose path

getStatus :: Bool -> FilePath -> IO FileType
getStatus verbose path = do
  stat <- getSymbolicLinkStatus path
  case (isSymbolicLink stat, isDirectory stat, isRegularFile stat) of
    (True, _, _) -> return Link
    (_, _, True) -> return Regular
    (_, True, _) -> do
      when verbose (putStrLn $ "getSubdirectoryFiles " ++ show path ++ ", status: " ++ show (isSymbolicLink stat, isDirectory stat, isRegularFile stat))
      return Folder
    _ -> return Other

getModificationTime :: FilePath -> IO EpochTime
getModificationTime path = modificationTime <$> getSymbolicLinkStatus path

makeChecksumMap :: Bool -> [FilePath] -> IO (Map Sum (Set FilePath))
makeChecksumMap verbose paths =
    Map.fromListWith union <$> mapM toPair paths
    where
      toPair path = do
        (ck, _bytes) <- fileChecksum path
        when verbose (putStrLn ("Checksum " ++ show path ++ " = " ++ show ck))
        return (ck, singleton path)

fileChecksum :: FilePath -> IO (Sum, ByteString)
fileChecksum path = do
  bytes <- BS.readFile path
  let ck = checksum bytes
  return (ck, bytes)

filterRegular :: [(FilePath, FileType)] -> [FilePath]
filterRegular = mapMaybe (\(path, typ) -> if typ == Regular then Just path else Nothing)

filterDuplicates :: Map Sum (Set FilePath) -> Map Sum (Set FilePath)
filterDuplicates = Map.filter (\s -> Set.size s > 1)
