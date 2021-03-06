{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, RankNTypes, ScopedTypeVariables, TemplateHaskell, TupleSections #-}
{-# OPTIONS -Wall #-}

module Find
    ( St(..)
    , Sum
    , FileAttribute(..), toSum, isRegular
    , FileComparison(..)
    , compareFolders
    , zipFolderFiles
    , compareFiles
    , makeLengthMap
    , findDuplicateFiles
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
import Control.Monad.State (evalStateT)
import Data.ByteString as BS (ByteString, isPrefixOf, readFile, take)
import Data.ByteString.Lazy (fromStrict)
#if 0
import Data.Digest.Pure.SHA
#else
import Data.Digest.Pure.MD5
#endif
import Data.List (intercalate)
import Data.Map.Strict as Map (delete, elems, filter, fromList, fromListWith, lookup, Map, size, toList, unionsWith)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Set as Set (empty, fromList, insert, map, member, Set, singleton, size, toList, union, unions)
import Data.Tree
import System.Directory (canonicalizePath, getDirectoryContents)
import System.Posix.Files (fileSize, getSymbolicLinkStatus, isDirectory, isRegularFile, isSymbolicLink, modificationTime)
import System.Posix.Types (EpochTime, FileOffset)
import System.FilePath ((</>))
import System.IO (hPutStr, stderr)
import System.IO.Error (isDoesNotExistError)
-- import System.Posix (FileStatus)
-- import System.Posix.Types (EpochTime)

-- data FileType = Regular | Folder | Link | Other deriving (Show, Eq, Ord)

import Control.Lens (makeLenses, (%=), Lens', use)
import Control.Monad.State (MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Default

data St = St {_statCount :: Int, _readCount :: Int} deriving (Eq, Ord, Show)
$(makeLenses ''St)

instance Default St where def = St 0 0

class IsFolder a where
    isFolder :: a -> Bool

instance IsFolder (Set FileAttribute) where
    isFolder = Set.member Folder

data FileAttribute
    = Nonexistant
    | Unreadable
    | Regular
    | Folder
    | Link
    | Other
    | Length FileOffset
    | ModTime EpochTime
    -- This should be computed only if necessary, and removed from the
    -- Set ASAP to avoid using vast amounts of RAM.
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
  show (Length l) = "Length " ++ show l
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

{- The idea of "folding" in haskell seems pretty straightforward - you
   give it an a, and some functions to apply to the different pieces of
   an a to make an r, and the fold applies the appropriate function or
   functions to create a result:

     foldr :: (a -> r -> r) -> r -> t a -> r

   Unfolding, on the other hand, is much more slippery.  Presumably it
   is the inverse operation to folding, so it takes a "result" r and
   builds an a.  First problem is that the word "unfold" makes me think
   of the exact opposite.  In my mind, the "unfold" operation for a tree
   is going to give me something other than a tree.  But in Haskell world
   doing a Tree unfold is what builds you a Tree.  So lets look:

     unfoldTree :: (b -> (a, [b])) -> b -> Tree a
     unfoldTreeM :: Monad m => (b -> m (a, [b])) -> b -> m (Tree a)

   This is clearly going to turn a @b@ into a @Tree a@, and it is going to
   use a function parameter that turns any @b@ into an @a@ (a node label) and
   a list of @b@, which presumably represent the tree's children. -}

-- | Do a deep comparison of the contents of two folders.
compareFolders :: (MonadIO m, MonadState St m) => Int -> FilePath -> FilePath -> m (Tree (FilePath, Set FileComparison))
compareFolders verbosity left0 right0 = do
  left <- liftIO $ canonicalizePath left0
  right <- liftIO $ canonicalizePath right0
  if left == right
  then (error $ "Cannot compare a directory to itself: " ++ show left0 ++ ", " ++ show right0)
  else do
    -- Build a tree with comparative meta information
    (tree :: Tree (FilePath, (Set FileAttribute, Set FileAttribute))) <-
        zipFolderFiles verbosity (getStatus verbosity) (\a b -> return (a, b)) left right
    -- Get additional information where there are corresponding file
    when (verbosity >= 1) $ liftIO $ putStrLn $
      "Number of regular files to read: " ++
        show (2 * (length (Prelude.filter (\(_, (lattrs, rattrs)) -> Set.member Regular lattrs && Set.member Regular rattrs) (flatten tree))))
    (tree' :: Tree (FilePath, Set FileComparison)) <-
        unfoldTreeM (\(Node (path, (lattrs, rattrs)) subforest) ->
                         if (Set.member Regular lattrs && Set.member Regular rattrs)
                         then do
                           lattrs' <- getDeeper verbosity (left </> path) lattrs
                           rattrs' <- getDeeper verbosity (right </> path) rattrs
                           when (verbosity >= 2) (liftIO $ putStrLn ("lattrs' " ++ path ++ ": " ++ show lattrs'))
                           when (verbosity >= 2) (liftIO $ putStrLn ("rattrs' " ++ path ++ ": " ++ show rattrs'))
                           let attrs = compareFiles lattrs' rattrs'
                           when (verbosity >= 2) (liftIO $ putStrLn ("attrs " ++ path ++ ": " ++ show attrs))
                           return $ ((path, attrs), subforest)
                         else return $ ((path, compareFiles lattrs rattrs), subforest)) tree
    when (verbosity >= 3) (liftIO $ putStrLn (show tree'))
    return tree'

-- | Traverse two folders and collect information about the
-- corresponding files at corresponding subpaths.
zipFolderFiles ::
       (IsFolder attributes, Show attributes, MonadIO m, MonadState St m)
    => Int
    -> (FilePath -> m attributes)
    -> (attributes -> attributes -> m a)
    -> FilePath
    -> FilePath
    -> m (Tree (FilePath, a))
zipFolderFiles verbosity get fn ltop rtop =
    work ""
    where
      work sub = do
        lattrs <- get (ltop </> sub)
        rattrs <- get (rtop </> sub)
        when (verbosity >= 2) $ do
          liftIO $ do
            putStrLn ("lattrs " ++ sub ++ ": " ++ show lattrs)
            putStrLn ("rattrs " ++ sub ++ ": " ++ show rattrs)
        sf <- case (isFolder lattrs, isFolder rattrs) of
          (True, True) -> do
            (lsubs :: Set FilePath) <- (Set.fromList . fmap (sub </>)) <$> listDirectory (ltop </> sub)
            (rsubs :: Set FilePath) <- (Set.fromList . fmap (sub </>)) <$> listDirectory (rtop </> sub)
            let subs = union lsubs rsubs
            mapM work (Set.toList subs)
          _ -> return []
        a <- fn lattrs rattrs
        return $ Node (sub, a) sf

listDirectory :: (MonadIO m, MonadState St m) => FilePath -> m [FilePath]
listDirectory path = Prelude.filter (`notElem` [".", ".."]) <$> liftIO (getDirectoryContents path)

-- | Retrieve meta information about a file.
getStatus :: (MonadIO m, MonadState St m) => Int -> FilePath -> m (Set FileAttribute)
getStatus _verbosity path = do
  estat <- liftIO $ try (getSymbolicLinkStatus path)
  doDots 10000 "stats" statCount
  case estat of
    Left e | isDoesNotExistError e -> return $ singleton Nonexistant
    Left e -> throw e
    Right stat -> do
      case (isSymbolicLink stat, isDirectory stat, isRegularFile stat) of
        (True, _, _) -> return $ singleton Link
        (_, True, _) -> return $ singleton Folder
        (_, _, True) -> return $ Set.fromList [Regular, ModTime (modificationTime stat), Length (fileSize stat)]
        _ -> return $ singleton Other

-- | In addition to 'getStatus' result retrieve the file's content as a 'ByteString'.
getStatusDeep :: (MonadIO m, MonadState St m) => Int -> FilePath -> m (Set FileAttribute)
getStatusDeep verbosity path = getStatus verbosity path >>= getDeeper verbosity path

-- | Add deep status information to a file's set of meta information.
getDeeper :: (MonadIO m, MonadState St m) => Int -> FilePath -> (Set FileAttribute) -> m (Set FileAttribute)
getDeeper _verbosity path attrs = do
  content <- liftIO $ try (BS.readFile path)
  doDots 1000 "reads" readCount
  return $ either (\(_ :: IOError) -> Set.insert Unreadable attrs)
                  (\bytes -> Set.insert (Bytes bytes) ({-Set.insert (Checksum ck)-} attrs))
                  content

-- | Use a state monad to output message whenever the lens
-- state value reaches a multiple of count.
doDots :: (MonadIO m, MonadState St m) => Int -> String -> Lens' St Int -> m ()
doDots count message lens = do
  lens %= succ
  n <- use lens
  when (n `mod` count == 0) (liftIO $ hPutStr stderr (show n ++ " " ++ message ++ "..."))

-- | Retrieve all the files within a directory using the supplied get
-- function (typically 'getStatus' or getStatusDeep') and then apply
-- @f@ to the retrieved attributes.  This is to avoid the collection
-- of enormous quantities of data during the traversal.
getSubdirectoryFilesRecursive ::
    forall m a attributes. (IsFolder attributes, MonadIO m, MonadState St m, Show a)
    => Int
    -> (FilePath -> m attributes) -- ^ Must at least determine if path is a 'Folder'
    -> (attributes -> a)
    -> FilePath
    -> m (Map FilePath a)
getSubdirectoryFilesRecursive verbosity get f top =
    Map.fromList <$> getSubdirectoryFilesRecursive' top >>= r1
    where
      r1 mp = when (verbosity >= 1) (liftIO $ putStrLn ("getSubdirectoryFilesRecursive - found " ++ show (Map.size mp) ++ " files")) >> return mp

      getSubdirectoryFilesRecursive' :: (MonadIO m, MonadState St m) => FilePath -> m [(FilePath, a)]
      getSubdirectoryFilesRecursive' path = do
        when (verbosity >= 2) $ liftIO $ putStrLn ("  " ++ path)
        attrs <- get path
        subpaths <- getSubdirectoryFiles path attrs
        subfiles <- concat <$> mapM (getSubdirectoryFilesRecursive') subpaths
        return $ (path, f attrs) : subfiles

      getSubdirectoryFiles :: (MonadIO m, MonadState St m) => FilePath -> attributes -> m [FilePath]
      getSubdirectoryFiles path attrs = do
        case isFolder attrs of
          True -> fmap (path </>) <$> listDirectory path
          False -> return []

findDuplicateFiles :: (MonadIO m, MonadState St m) => Int -> [FilePath] -> m (Map (Maybe Sum) (Set FilePath))
findDuplicateFiles verbosity tops = do
  when (verbosity >= 1) (liftIO $ putStrLn $ "Searching for duplicate files in " ++ show tops)
  (trees :: [Map FilePath (Set FileAttribute)]) <- flip evalStateT def $
              mapM (\top -> getSubdirectoryFilesRecursive verbosity (getStatus verbosity) id top) tops
  let tree = (Map.unionsWith (\_ _ -> error "unions") trees :: Map FilePath (Set FileAttribute))
  let lmp = makeLengthMap verbosity tree
  makeChecksumMap verbosity tree lmp

-- | Given a map of the file attributes and a map of the files
-- collected by size, build a map from checksum to paths.
makeChecksumMap ::
    forall m. (MonadIO m, MonadState St m)
    => Int
    -> Map FilePath (Set FileAttribute)
    -> Map FileOffset (Set FilePath)
    -> m (Map (Maybe Sum) (Set FilePath))
makeChecksumMap verbosity attrmap lengthmap = do
  when (verbosity >= 1) $ do
    liftIO $ putStrLn $
      "Number of files to compare: " ++ show (Set.size (unions (elems lengthmap'))) ++
      "\n total size: " ++ show (sum (fmap (\(l, s) -> l * toEnum (Set.size s)) (Map.toList lengthmap')))
  when (verbosity >= 2) $ do
    maybe (return ()) (\s -> liftIO $ putStrLn $ " # zero length files: " ++ show (Set.size s)) (Map.lookup 0 lengthmap')
    liftIO $ putStrLn $ "\n size groups: " ++ intercalate "\n  " sizeGroups
  Map.fromList <$> mapM getSum paths
    where
      sizeGroups :: [String]
      sizeGroups = fmap (\(l, s) -> intercalate "\n   " ((show l ++ ":") : Set.toList s)) (Map.toList (Map.delete 0 lengthmap'))
      paths :: [FilePath]
      paths = Set.toList (Set.unions (Map.elems lengthmap'))
      getSum :: FilePath -> m (Maybe Sum, Set FilePath)
      getSum path = do
        attrs' <- case Map.lookup path attrmap of
                    Just attrs -> getDeeper verbosity path attrs
                    Nothing -> error $ "makeChecksumMap - missing from attrmap: " ++ show path
        return $ (toSum attrs', singleton path)
      lengthmap' :: Map FileOffset (Set FilePath)
      lengthmap' = Map.filter (\s -> Set.size s > 1) lengthmap

-- | Collect files by size in a map.
makeLengthMap :: Int -> Map FilePath (Set FileAttribute) -> Map FileOffset (Set FilePath)
makeLengthMap _verbosity attrmap =
    Map.fromListWith union $ concatMap toPair $ Map.toList attrmap
    where
      toPair (path, attrs) = maybe [] (\l -> [(l, singleton path)]) (toLength attrs)

-- | Remove files that have no duplicates
keepDuplicates :: forall k. Ord k => Map k (Set FilePath) -> Map k (Set FilePath)
keepDuplicates = Map.filter (\s -> Set.size s > 1)

-- | Remove files that are not regular - folders, devices, links, etc..
keepRegular :: Map FilePath (Set FileAttribute) -> Map FilePath (Set FileAttribute)
keepRegular = Map.filter (Set.member Regular)

isRegular :: Set FileAttribute -> Bool
isRegular = Set.member Regular
toBytes :: Set FileAttribute -> Maybe ByteString
toBytes = listToMaybe . catMaybes . Set.toList . Set.map (\x -> case x of Bytes b -> Just b; _ -> Nothing)
toModTime :: Set FileAttribute -> Maybe EpochTime
toModTime = listToMaybe . catMaybes . Set.toList . Set.map (\x -> case x of ModTime t -> Just t; _ -> Nothing)
toLength :: Set FileAttribute -> Maybe FileOffset
toLength = listToMaybe . catMaybes . Set.toList . Set.map (\x -> case x of Length t -> Just t; _ -> Nothing)
-- toLength :: Set FileAttribute -> Maybe Int
-- toLength = fmap BS.length . toBytes
toSum :: Set FileAttribute -> Maybe Sum
toSum = fmap checksum . toBytes

-- | Use two sets of file attributes to generate the comparison set.
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
