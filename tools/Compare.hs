-- | Describe the relationship between two folders Master and Slave:
--
--      1. A and B are identical
--      2. A and B are identical except some files in Master are missing from Slave
--      3. A and B are identical except some files in Slave are missing from Master
--      4. A and B are identical each have files missing from the other
--      5. Some files common to A and B differ
--
-- Conditions 1 thru 4 are all mutually exclusive.  Condition 5 can
-- accompany any of conditions 1 thru 4.

{-# LANGUAGE DeriveFunctor, ScopedTypeVariables #-}

--import Debug.Trace
import Control.Monad (when)
import Control.Monad.Extra (partitionM)
import qualified Data.ByteString.Lazy as BS (isPrefixOf, length)
--import Data.Digest.Pure.SHA
--import Data.Digest.Pure.MD5
-- import Data.List (partition)
import Data.Map.Strict as Map (fromListWith, keys, partition)
--import Data.Maybe (mapMaybe)
import Data.Set as Set (empty, Set, singleton, unions)
import Find
import Options.Applicative
import System.Directory (canonicalizePath)
import System.Posix.Files (getSymbolicLinkStatus, modificationTime)
--import System.Environment (withArgs)
import System.FilePath ((</>), addTrailingPathSeparator)
import Text.PrettyPrint.ANSI.Leijen (Doc, vcat, text)

newtype Master a = Master a deriving (Show, Eq, Ord, Functor)
newtype Slave a = Slave a deriving (Show, Eq, Ord, Functor)

-- withArgs ["-v", "--master", "/mnt/sda2/srv/originals/mail/mail5", "--slave", "/mnt/sda2/srv/originals/mail/mail1"] main

main :: IO ()
main = do
  Options verbose master slave <- execParser (info (options <**> helper) (fullDesc <> progDescDoc (Just intro)))
  master' <- canonicalizePath master
  slave' <- canonicalizePath slave
  case slave' == master' of
    True -> error $ "Cannot compare a directory to itself: " ++ show master ++ ", " ++ show slave
    False -> go verbose master slave

intro :: Doc
intro =
  vcat $ fmap text $
    [ ""
    , "This program performs a detailed comparison of two directories,"
    , "somewhat arbitrarily labelled 'master' and 'slave'.  First it"
    , "checks that the two paths do not refer to the same folder, either"
    , "directly or indirectly via symbolic or hard links.  (It might be"
    , "possible to fool this test using mount --bind.)  It collects information"
    , "about all the regular files and subdirectories files in each file tree,"
    , "(not following symbolic links.)  It then outputs a summary, which includes:"
    , "   * the status of each distinct path in either tree, including whether"
    , "     the file is missing from slave, missing from master, or present in both"
    , "   * for files present in both tree, a set of relationship attributes are"
    , "     computed describing whether the files differ, how their lengths differ,"
    , "     whether either is a prefix of the other, and how their ages differ."
    , ""]

data Options = Options Bool FilePath FilePath

options :: Parser Options
options = Options <$> switch (long "verbose" <> short 'v' <> help "Give progress messages")
                  <*> strOption ( long "master" <> metavar "FOLDER" <> help "Master folder for comparison")
                  <*> strOption ( long "slave" <> metavar "FOLDER" <> help "Slave folder for comparison")

go :: Bool -> FilePath -> FilePath -> IO ()
go verbose master slave = do
  when verbose $ putStrLn $ "Comparing folders: " ++ show master ++ " vs. " ++ show slave
  (mpaths :: [FilePath]) <- getSubdirectoryFilesRecursive verbose master >>= {-makeChecksumMap verbose .-} return . fmap (drop (length (addTrailingPathSeparator master))) . filterRegular
  (spaths :: [FilePath]) <- getSubdirectoryFilesRecursive verbose slave >>= {-makeChecksumMap verbose .-} return . fmap (drop (length (addTrailingPathSeparator slave))) . filterRegular
  let mp = Map.fromListWith
             (\(ma, mb) (sa, sb) -> (ma || sa, mb || sb))
             (fmap (\p -> (p, (True, False))) mpaths ++
              fmap (\p -> (p, (False, True))) spaths)
  let (common, other) = partition (\pr -> case pr of
                                            (True, True) -> True
                                            _ -> False) mp
      (masterOnly, slaveOnly) = partition (\pr -> case pr of
                                                    (True, False) -> True
                                                    (False, True) -> False) other
  let masterOnly' = Map.keys masterOnly
      slaveOnly' = Map.keys slaveOnly
      common' = Map.keys common
  (equal, differing) <- partitionM (\sub -> equalFiles (master </> sub) (slave </> sub)) common'
  differing' <- zip differing <$> mapM (\sub -> compareFiles (master </> sub) (slave </> sub)) differing
  putStrLn $ describeStatus master slave masterOnly' slaveOnly' equal differing'
  putStrLn $ "status: " ++ show (status masterOnly' slaveOnly' equal differing')
    where
      mf :: a -> (Maybe a, Maybe a)
      mf a = (Just a, Nothing)
      sf :: a -> (Maybe a, Maybe a)
      sf a = (Nothing, Just a)
      combineLeaf :: (Show a, Show b) => (Maybe a, Maybe b) -> (Maybe a, Maybe b) -> (Maybe a, Maybe b)
      combineLeaf (Just a, Nothing) (Nothing, Just b) = (Just a, Just b)
      combineLeaf a b = error $ "Unexpected arguments to combineLeaf: " ++ show (a,  b)

-- | Comparing two files yields a set of these
data FileComparison =
    Identical
  | Differing
  | Longer
  | Shorter
  | SameLength
  | HasPrefix -- Fun fact: HasPrefix && IsPrefix <=> Identical
  | IsPrefix
  | Newer
  | SameAge
  | Older
  deriving (Show, Eq, Ord)

compareFiles :: FilePath -> FilePath -> IO (Set FileComparison)
compareFiles m s = do
  mstat <- getSymbolicLinkStatus m
  sstat <- getSymbolicLinkStatus s
  (mck, mbytes) <- fileChecksum m
  (mck, sbytes) <- fileChecksum s
  return $ Set.unions $ [doEquality mbytes sbytes,
                         doLength mbytes sbytes,
                         doPrefix mbytes sbytes,
                         doAge mstat sstat]
    where
      doEquality mbytes sbytes =
          case mbytes == sbytes of
            True -> singleton Identical
            False -> singleton Differing
      doLength mbytes sbytes =
          case compare (BS.length mbytes) (BS.length sbytes) of
            LT -> singleton Shorter
            EQ -> singleton SameLength
            GT -> singleton Longer

      doPrefix mbytes sbytes =
          case (BS.isPrefixOf sbytes mbytes, BS.isPrefixOf mbytes sbytes) of
            (True, True) -> Set.empty
            (True, False) -> singleton HasPrefix
            (False, True) -> singleton IsPrefix
            (False, False) -> Set.empty
      doAge mstat sstat = do
        case compare (modificationTime mstat) (modificationTime sstat) of
          LT -> singleton Older
          EQ -> singleton SameAge
          GT -> singleton Newer

equalFiles :: FilePath -> FilePath -> IO Bool
equalFiles a b = do
  (ck1, _) <- fileChecksum a
  (ck2, _) <- fileChecksum b
  return (ck1 == ck2)

status :: [FilePath] -> [FilePath] -> [FilePath] -> [(FilePath, Set FileComparison)] -> Int
status masterOnly slaveOnly equal differing =
    case (null masterOnly, null slaveOnly) of
      (True, True) -> 1
      (False, True) -> 2
      (True, False) -> 3
      (False, False) -> 4

describeStatus ::
       FilePath
    -> FilePath
    -> [FilePath]
    -> [FilePath]
    -> [FilePath]
    -> [(FilePath, Set FileComparison)]
    -> String
describeStatus master slave masterOnly slaveOnly equal differing =
    (if not (null differing) then unlines ("Common files differ:" : fmap show differing) else "") ++
    (case status masterOnly slaveOnly equal differing of
       1 -> "Folders file names are identical"
       2 -> unlines $ ("Missing from slave " ++ show slave ++ ":") : fmap (("  " ++) . show) masterOnly
       3 -> unlines $ ("Missing from master " ++ show master ++ ":") : fmap (("  " ++) . show) slaveOnly
       4 -> unlines $ (("Missing from slave " ++ show slave ++ ":") : fmap (("  " ++) . show) masterOnly) ++
                      (("Missing from master " ++ show master ++ ":") : fmap (("  " ++) . show) slaveOnly))
