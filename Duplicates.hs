{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}

-- | Find all duplicate regular files in a folder.

import Control.Monad (when)
import Data.Map.Strict as Map (elems, filter, Map, size)
import Data.Set as Set (Set, size, toList)
import Find (getSubdirectoryFilesRecursive, makeChecksumMap, filterRegular, filterDuplicates, Sum)
import Options.Applicative

main :: IO ()
main =
    execParser
      (info
         (options <**> helper)
         (fullDesc
          -- <> progDesc "Print a greeting for TARGET"
          -- <> header "hello - a test for optparse-applicative"
         )) >>= go

data Options = Options FilePath Bool

options :: Parser Options
options = Options <$> strOption ( long "top" <> metavar "FOLDER" <> help "Top folder to search for duplicates")
                  <*> switch (long "verbose" <> short 'v' <> help "Give progress messages")

go :: Options -> IO ()
go (Options top verbose) = do
  when verbose (putStrLn $ "Searching for duplicate files in " ++ top)
  getSubdirectoryFilesRecursive verbose top >>= makeChecksumMap verbose . filterRegular >>= reportDuplicates . filterDuplicates

reportDuplicates :: Map Sum (Set FilePath) -> IO ()
reportDuplicates mp = do
  putStr (show (Map.size mp) ++ " sets of duplicates found -\n" ++ unlines messages)
    where
      messages :: [String]
      messages = fmap dupMessage dups
      dups :: [Set FilePath]
      dups = Map.elems $ Map.filter (\s -> Set.size s > 1) mp
      dupMessage :: Set FilePath -> String
      dupMessage paths = unlines ("duplicates:" : fmap ("  " ++) (Set.toList paths))
