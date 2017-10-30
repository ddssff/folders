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
import Control.Monad.State (evalStateT)
import Control.Monad.Trans (liftIO)
import Data.Default (def)
import Data.Set as Set (member, notMember, Set)
import Data.Tree
import Find
import Options.Applicative
import System.Environment (withArgs)
import Text.PrettyPrint.ANSI.Leijen (Doc, vcat, text)

data Options = Options {_verbosity :: Int, _left :: FilePath, _right :: FilePath} deriving (Show, Eq, Ord)

$(makeLenses ''Options)

-- withArgs ["-v", "1", "--left", "/mnt/sda2/pCloudSync/lydia/Documents01/Photos", "--right", "/mnt/sda2/pCloudSync/lydia/Documents01/My Photos"] main
-- dist/built/compare/compare -v 1 --left "/mnt/sda2/pCloudSync/lydia/Documents01/Photos" --right "/mnt/sda2/pCloudSync/lydia/Documents01/My Photos"

main :: IO ()
main = do
  opts <- execParser (info (options <**> helper) (fullDesc <> progDescDoc (Just intro)))
  tree <- evalStateT (compareFolders (view verbosity opts) (view left opts) (view right opts)) def
  report tree

report :: Tree (FilePath, Set FileComparison) -> IO ()
report tree =
    liftIO $ do
      putStrLn ("# files: " ++ show (length pairs))
      putStrLn (unlines ("differing:" : fmap show (filter ((\s -> Set.member BothRegular s && Set.notMember Identical s) . snd) pairs)))
      putStrLn ("left only: " ++ show (fmap fst (filter (Set.member LeftExistsOnly . snd) pairs)))
      putStrLn ("right only: " ++ show (fmap fst (filter (Set.member RightExistsOnly . snd) pairs)))
    where
      pairs :: [(FilePath, Set FileComparison)]
      pairs = flatten tree

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

