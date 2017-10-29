-- | Describe the relationship between two folders Left and Right:
--
--      1. A and B are identical
--      2. A and B are identical except some files in Left are missing from Right
--      3. A and B are identical except some files in Right are missing from Left
--      4. A and B are identical each have files missing from the other
--      5. Some files common to A and B differ
--
-- Conditions 1 thru 4 are all mutually exclusive.  Condition 5 can
-- accompany any of conditions 1 thru 4.
--
-- Features:
--   * Only looks at contents when file exists in both trees.  So comparing
--     an enormous file tree to a reasonable size file tree is feasable.

{-# LANGUAGE CPP, DeriveFunctor, FlexibleContexts, ScopedTypeVariables, TemplateHaskell #-}

import Control.Lens (makeLenses, view)
import Control.Monad (when)
import Control.Monad.State (evalStateT, MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Default (def)
import Data.Set as Set (member, notMember, Set)
import Data.Tree
import Find
import Options.Applicative
import System.Directory (canonicalizePath)
import System.Environment (withArgs)
import System.FilePath ((</>))
import Text.PrettyPrint.ANSI.Leijen (Doc, vcat, text)

data Options = Options {_verbosity :: Int, _left :: FilePath, _right :: FilePath} deriving (Show, Eq, Ord)

$(makeLenses ''Options)

-- withArgs ["-v", "1", "--left", "/mnt/sda2/pCloudSync/Audio/Music/Akon", "--right", "/mnt/sda2/srv/originals/audio/iTunes Music"] main

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) (fullDesc <> progDescDoc (Just intro)))
  left' <- canonicalizePath (view left opts)
  right' <- canonicalizePath (view right opts)
  case right' == left' of
    True -> error $ "Cannot compare a directory to itself: " ++ show (view left opts) ++ ", " ++ show (view right opts)
    False -> evalStateT (go opts) def

intro :: Doc
intro =
  vcat $ fmap text $
    [ ""
    , "This program performs a detailed comparison of two directories,"
    , "somewhat arbitrarily labelled 'left' and 'right'.  First it"
    , "checks that the two paths do not refer to the same folder, either"
    , "directly or indirectly via symbolic or hard links.  (It might be"
    , "possible to fool this test using mount --bind.)  It collects information"
    , "about all the regular files and subdirectories files in each file tree,"
    , "(not following symbolic links.)  It then outputs a summary, which includes:"
    , "   * the status of each distinct path in either tree, including whether"
    , "     the file is missing from right, missing from left, or present in both"
    , "   * for files present in both tree, a set of relationship attributes are"
    , "     computed describing whether the files differ, how their lengths differ,"
    , "     whether either is a prefix of the other, and how their ages differ."
    , ""]

options :: Parser Options
options = Options <$> option auto (long "verbosity" <> short 'v' <> help "Amount of progress messages" <> showDefault <> value 0 <> metavar "INT")
                  <*> strOption (long "left" <> metavar "FOLDER" <> help "Left folder for comparison")
                  <*> strOption (long "right" <> metavar "FOLDER" <> help "Right folder for comparison")

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

go :: (MonadIO m, MonadState St m) => Options -> m ()
go opts = do
#if 1
    -- Build a tree with comparative meta information
    (tree :: Tree (FilePath, (Set FileAttribute, Set FileAttribute))) <-
        zipFolderFiles (view verbosity opts) (\a b -> return (a, b)) (view left opts) (view right opts)
    -- Get additional information where there are corresponding file
    when (view verbosity opts >= 1) $ liftIO $ putStrLn $
      "Number of regular files to read: " ++
        show (2 * (length (Prelude.filter (\(_, (lattrs, rattrs)) -> Set.member Regular lattrs && Set.member Regular rattrs) (flatten tree))))
    (tree' :: Tree (FilePath, Set FileComparison)) <-
        unfoldTreeM (\(Node (path, (lattrs, rattrs)) subforest) ->
                         if (Set.member Regular lattrs && Set.member Regular rattrs)
                         then do
                           lattrs' <- getDeeper (view verbosity opts) (view left opts </> path) lattrs
                           rattrs' <- getDeeper (view verbosity opts) (view right opts </> path) rattrs
                           when (view verbosity opts >= 2) (liftIO $ putStrLn ("lattrs' " ++ path ++ ": " ++ show lattrs'))
                           when (view verbosity opts >= 2) (liftIO $ putStrLn ("rattrs' " ++ path ++ ": " ++ show rattrs'))
                           let attrs = compareFiles lattrs' rattrs'
                           when (view verbosity opts >= 2) (liftIO $ putStrLn ("attrs " ++ path ++ ": " ++ show attrs))
                           return $ ((path, attrs), subforest)
                         else return $ ((path, compareFiles lattrs rattrs), subforest)) tree
    when (view verbosity opts >= 3) (liftIO $ putStrLn (show tree'))
    let pairs = flatten tree'
    liftIO $ do
      putStrLn ("# files: " ++ show (length pairs))
      putStrLn (unlines ("differing:" : fmap show (filter ((\s -> Set.member BothRegular s && Set.notMember Identical s) . snd) pairs)))
      putStrLn ("left only: " ++ show (fmap fst (filter (Set.member LeftExistsOnly . snd) pairs)))
      putStrLn ("right only: " ++ show (fmap fst (filter (Set.member RightExistsOnly . snd) pairs)))
#else
  when verbose $ putStrLn $ "Comparing folders: " ++ show left ++ " vs. " ++ show right
  (mpaths :: [FilePath]) <- getSubdirectoryFilesRecursive verbose (getStatus verbose) id left >>=
                            return . fmap (drop (length (addTrailingPathSeparator left))) . Map.keys . keepRegular
  -- hPutStrLn stderr $ "# files in " ++ show left ++ ": " ++ show (length mpaths)
  (spaths :: [FilePath]) <- getSubdirectoryFilesRecursive verbose (getStatus verbose) id right >>=
                            return . fmap (drop (length (addTrailingPathSeparator right))) . Map.keys . keepRegular
  -- hPutStrLn stderr $ "# files in " ++ show right ++ ": " ++ show (length spaths)
  let mp :: Map FilePath (Bool, Bool)
      mp = Map.fromListWith
             (\(ma, mb) (sa, sb) -> (ma || sa, mb || sb))
             (fmap (\p -> (p, (True, False))) mpaths ++
              fmap (\p -> (p, (False, True))) spaths)
  -- Which files exist in both trees?
  let (common, other) = partition (\pr -> case pr of
                                            (True, True) -> True
                                            _ -> False) mp
      (leftOnly, rightOnly) = partition (\pr -> case pr of
                                                    (True, False) -> True
                                                    (False, True) -> False) other
  let leftOnly' = Map.keys leftOnly
      rightOnly' = Map.keys rightOnly
      common' = Map.keys common
  putStrLn $ "# common: " ++ show (length common')
  putStrLn $ "# non-common: " ++ show (length (leftOnly' ++ rightOnly'))
  -- Compare common files
  mp <- filePairInfoMap left right common'
  hPutStrLn stderr "b"
  let (equal, differing) = partition (Set.member Identical) mp
  putStrLn $ describeStatus left right leftOnly' rightOnly' (Map.keys equal) differing
  putStrLn $ "status: " ++ show (status leftOnly' rightOnly' (Map.keys equal) differing)
    where
      mf :: a -> (Maybe a, Maybe a)
      mf a = (Just a, Nothing)
      sf :: a -> (Maybe a, Maybe a)
      sf a = (Nothing, Just a)
      combineLeaf :: (Show a, Show b) => (Maybe a, Maybe b) -> (Maybe a, Maybe b) -> (Maybe a, Maybe b)
      combineLeaf (Just a, Nothing) (Nothing, Just b) = (Just a, Just b)
      combineLeaf a b = error $ "Unexpected arguments to combineLeaf: " ++ show (a,  b)
#endif

#if 0
status :: [FilePath] -> [FilePath] -> [FilePath] -> Map FilePath (Set FileComparison) -> Int
status leftOnly rightOnly equal differing =
    case (Prelude.null leftOnly, Prelude.null rightOnly) of
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
    -> Map FilePath (Set FileComparison)
    -> String
describeStatus left right leftOnly rightOnly equal differing =
    (if not (Map.null differing) then unlines ("Common files differ:" : fmap show (sort (Map.keys differing))) else "") ++
    (case status leftOnly rightOnly equal differing of
       1 -> "Folders file names are identical"
       2 -> unlines $ ("Missing from right " ++ show right ++ ":") : fmap (("  " ++) . show) leftOnly
       3 -> unlines $ ("Missing from left " ++ show left ++ ":") : fmap (("  " ++) . show) rightOnly
       4 -> unlines $ (("Missing from right " ++ show right ++ ":") : fmap (("  " ++) . show) leftOnly) ++
                      (("Missing from left " ++ show left ++ ":") : fmap (("  " ++) . show) rightOnly))
#endif
