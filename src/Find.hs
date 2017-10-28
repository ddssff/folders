{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
{-# OPTIONS -Wall #-}

module Find
    ( Sum
    , FileAttribute(..), toSum, isRegular
    , FileComparison(..)
    , zipFolderFiles
    , compareFiles
    , makeChecksumMap
    , keepDuplicates
    , keepRegular
    -- , fileChecksum
    , getSubdirectoryFilesRecursive
    , getStatus
    , getDeeper
    , getStatusDeep
    ) where

import Control.Exception (throw, try)
import Control.Monad (when)
import Data.ByteString as BS (ByteString, isPrefixOf, length, readFile, take)
import Data.ByteString.Lazy (fromStrict)
#if 0
import Data.Digest.Pure.SHA
#else
import Data.Digest.Pure.MD5
#endif
import Data.Map.Strict as Map (filter, fromList, fromListWith, Map, size, toList)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set as Set (empty, fromList, insert, map, member, Set, singleton, size, toList, union, unions)
import Data.Tree
import System.Directory (getDirectoryContents)
import System.Posix.Files (getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink, modificationTime)
import System.Posix.Types (EpochTime)
import System.FilePath ((</>))
-- import System.IO (hPutStr, stderr)
import System.IO.Error (isDoesNotExistError)
-- import System.Posix (FileStatus)
-- import System.Posix.Types (EpochTime)

-- data FileType = Regular | Folder | Link | Other deriving (Show, Eq, Ord)

data FileAttribute
    = Nonexistant
    | Unreadable
    | Regular
    | Folder
    | Link
    | Other
    -- These should be computed only if necessary, and removed from the
    -- Set ASAP to avoid using vast amounts of RAM.
    | ModTime EpochTime
    | Bytes ByteString
    -- | Checksum Sum
    deriving (Eq, Ord)

instance Show FileAttribute where
  show Nonexistant = "Nonexistant"
  show Unreadable = "Unreadable"
  show Regular = "Regular"
  show Folder = "Folder"
  show Link = "Link"
  show Other = "Other"
  show (ModTime t) = "ModTime " ++ show t
  show (Bytes bs) = "Bytes " ++ show (BS.take 20 bs)
  -- show (Checksum ck) = "Checksum " ++ show ck

-- | Comparing two files yields a set of these
data FileComparison =
  -- Fun facts: RightIsPrefix && LeftIsPrefix <=> Identical
    Identical | RightIsPrefix | LeftIsPrefix | Differing
  | LeftExistsOnly | RightExistsOnly | BothExist
  | Longer | Shorter | SameLength
  | Newer | SameAge | Older
  | LeftIsUnreadable | RightIsUnreadable | BothUnreadable
  | LeftIsRegular | RightIsRegular | BothRegular
  | LeftIsFolder | RightIsFolder | BothFolders
  deriving (Show, Eq, Ord)

#if 0
-- Slower than md5, and probably overkill
type Sum = Digest SHA256State
checksum :: ByteString -> Sum
checksum = sha256
#else
type Sum = MD5Digest
checksum :: ByteString -> Sum
checksum = md5 . fromStrict
#endif

-- | Traverse two folders and collect information about the
-- corresponding files at corresponding subpaths.
zipFolderFiles ::
       Bool
    -> (Set FileAttribute -> Set FileAttribute -> IO a)
    -> FilePath
    -> FilePath
    -> IO (Tree (FilePath, a))
zipFolderFiles verbose fn ltop rtop =
    work ""
    where
      work sub = do
        lattrs <- getStatus verbose (ltop </> sub)
        rattrs <- getStatus verbose (rtop </> sub)
        when verbose $ putStrLn ("lattrs " ++ sub ++ ": " ++ show lattrs)
        when verbose $ putStrLn ("rattrs " ++ sub ++ ": " ++ show rattrs)
        sf <- case (Set.member Folder lattrs, Set.member Folder rattrs) of
          (True, True) -> do
            (lsubs :: Set FilePath) <- (Set.fromList . fmap (sub </>)) <$> listDirectory (ltop </> sub)
            (rsubs :: Set FilePath) <- (Set.fromList . fmap (sub </>)) <$> listDirectory (rtop </> sub)
            let subs = union lsubs rsubs
            mapM work (Set.toList subs)
          _ -> return []
        a <- fn lattrs rattrs
        return $ Node (sub, a) sf

listDirectory :: FilePath -> IO [FilePath]
listDirectory path = Prelude.filter (`notElem` [".", ".."]) <$> getDirectoryContents path

-- | Retrieve meta information about a file.
getStatus :: Bool -> FilePath -> IO (Set FileAttribute)
getStatus _verbose path = do
  estat <- try (getSymbolicLinkStatus path)
  case estat of
    Left e | isDoesNotExistError e -> return $ singleton Nonexistant
    Left e -> throw e
    Right stat -> do
      case (isSymbolicLink stat, isDirectory stat, isRegularFile stat) of
        (True, _, _) -> return $ singleton Link
        (_, True, _) -> return $ singleton Folder
        (_, _, True) -> return $ Set.fromList [Regular, ModTime (modificationTime stat)]
        _ -> return $ singleton Other

-- | In addition to 'getStatus' result retrieve the file's content as a 'ByteString'.
getStatusDeep :: Bool -> FilePath -> IO (Set FileAttribute)
getStatusDeep verbose path = getStatus verbose path >>= getDeeper verbose path

-- | Add deep status information to a file's set of meta information.
getDeeper :: Bool -> FilePath -> (Set FileAttribute) -> IO (Set FileAttribute)
getDeeper _verbose path attrs = do
  content <- try (BS.readFile path)
  return $ either (\(_ :: IOError) -> Set.insert Unreadable attrs)
                  (\bytes -> Set.insert (Bytes bytes) ({-Set.insert (Checksum ck)-} attrs))
                  content

getSubdirectoryFilesRecursive ::
    forall a. Show a
    => Bool
    -> (FilePath -> IO (Set FileAttribute)) -- ^ Must at least determine if path is a 'Folder'
    -> (Set FileAttribute -> a)
    -> FilePath
    -> IO (Map FilePath a)
getSubdirectoryFilesRecursive verbose get f top =
    (Map.fromList <$> getSubdirectoryFilesRecursive' top) >>= r1
    where
      r1 mp = when verbose (putStrLn ("getSubdirectoryFilesRecursive - found " ++ show (Map.size mp) ++ " files")) >> return mp

      getSubdirectoryFilesRecursive' :: FilePath -> IO [(FilePath, a)]
      getSubdirectoryFilesRecursive' path = do
        when verbose $ putStrLn ("  " ++ path)
        attrs <- get path
        subpaths <- getSubdirectoryFiles path attrs
        subfiles <- concat <$> mapM (getSubdirectoryFilesRecursive') subpaths
        return $ (path, f attrs) : subfiles

      getSubdirectoryFiles :: FilePath -> Set FileAttribute -> IO [FilePath]
      getSubdirectoryFiles path attrs = do
        case Set.member Folder attrs of
          True -> (fmap (path </>) . Prelude.filter (`notElem` [".", ".."])) <$> getDirectoryContents path
          False -> return []

makeChecksumMap :: Bool -> Map FilePath Sum -> Map Sum (Set FilePath)
makeChecksumMap _verbose paths =
    Map.fromListWith union $ fmap toPair $ Map.toList paths
    where
      toPair (path, ck) = (ck, singleton path)

-- | Remove files that have no duplicates
keepDuplicates :: forall k. Ord k => Map k (Set FilePath) -> Map k (Set FilePath)
keepDuplicates = Map.filter (\s -> Set.size s > 1)

keepRegular :: Map FilePath (Set FileAttribute) -> Map FilePath (Set FileAttribute)
keepRegular = Map.filter (Set.member Regular)

#if 0
mergeIdenticalFolders :: Map FilePath (Set FileComparison) -> Map FilePath (Set FileComparison)
mergeIdenticalFolders mp = mp

filePairInfoMap :: FilePath -> FilePath -> IO (Map FilePath (Maybe (Set FileAttribute), Maybe (Set FileAttribute)))
filePairInfoMap ltop rtop = do
  (lfiles :: Map FilePath (Set FileAttribute)) <- getSubdirectoryFilesRecursive False ltop
  (rfiles :: Map FilePath (Set FileAttribute)) <- getSubdirectoryFilesRecursive False rtop
  return $ Map.unionWith merge (fmap (\x -> (Just x, Nothing)) lfiles) (fmap (\x -> (Nothing, Just x)) rfiles)
    where
      merge :: (Maybe (Set FileAttribute), Maybe (Set FileAttribute)) -> (Maybe (Set FileAttribute), Maybe (Set FileAttribute)) -> (Maybe (Set FileAttribute), Maybe (Set FileAttribute))
      merge (Just ls, Nothing) (Nothing, Just rs) = (Just ls, Just rs)
      merge _ _ = error "unexpected"
#endif

isRegular :: Set FileAttribute -> Bool
isRegular = Set.member Regular
toBytes :: Set FileAttribute -> Maybe ByteString
toBytes = listToMaybe . catMaybes . Set.toList . Set.map (\x -> case x of Bytes b -> Just b; _ -> Nothing)
toModTime :: Set FileAttribute -> Maybe EpochTime
toModTime = listToMaybe . catMaybes . Set.toList . Set.map (\x -> case x of ModTime t -> Just t; _ -> Nothing)
toSum :: Set FileAttribute -> Maybe Sum
toSum = fmap checksum . toBytes
toLength :: Set FileAttribute -> Maybe Int
toLength = fmap BS.length . toBytes

compareFiles :: Set FileAttribute -> Set FileAttribute -> Set FileComparison
compareFiles lattrs rattrs =
  Set.unions [doExists, doRegular, doFolders, doLength, doPrefix, doUnreadable, doAge]
    where
      doExists =
          case (Set.member Nonexistant lattrs, Set.member Nonexistant rattrs) of
            (False, False) -> singleton BothExist
            (True, False) -> singleton RightExistsOnly
            (False, True) -> singleton LeftExistsOnly
            _ -> Set.empty
      doFolders =
          case (Set.member Folder lattrs, Set.member Folder rattrs) of
            (True, True) -> singleton BothFolders
            (False, True) -> singleton RightIsFolder
            (True, False) -> singleton LeftIsFolder
            _ -> Set.empty
      doRegular =
          case (isRegular lattrs, isRegular rattrs) of
            (True, True) -> singleton BothRegular
            (True, False) -> singleton LeftIsRegular
            (False, True) -> singleton RightIsRegular
            _ -> Set.empty
      doLength =
          case (isRegular lattrs, toLength lattrs, isRegular rattrs, toLength rattrs) of
            (True, Just l, True, Just r) ->
                case compare l r of
                  LT -> singleton Shorter
                  EQ -> singleton SameLength
                  GT -> singleton Longer
            _ -> Set.empty

      doPrefix =
          case (toBytes lattrs, toBytes rattrs) of
            (Just lbytes, Just rbytes) ->
                case (BS.isPrefixOf rbytes lbytes, BS.isPrefixOf lbytes rbytes) of
                  (True, True) -> singleton Identical
                  (True, False) -> singleton RightIsPrefix
                  (False, True) -> singleton LeftIsPrefix
                  (False, False) -> Set.empty
            _ -> Set.empty

      doUnreadable =
          case (Set.member Unreadable lattrs, Set.member Unreadable rattrs) of
            (True, True) -> singleton BothUnreadable
            (True, False) -> singleton LeftIsUnreadable
            (False, True) -> singleton RightIsUnreadable
            _ -> Set.empty

      doAge = do
        case (toModTime lattrs, toModTime rattrs) of
          (Just lmtime, Just rmtime) ->
              case compare lmtime rmtime of
                LT -> singleton Older
                EQ -> singleton SameAge
                GT -> singleton Newer
          _ -> Set.empty
