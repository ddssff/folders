-- | Find all duplicate regular files in a folder.

{-# LANGUAGE TupleSections #-}
{-# OPTIONS -Wall #-}

import Control.Monad (when)
import Data.Map.Strict as Map (elems, filter, Map, mapMaybe, size)
import Data.Set as Set (Set, size, toList)
import Find (FileAttribute, getStatusDeep, getSubdirectoryFilesRecursive, isRegular, makeChecksumMap, keepDuplicates, Sum, toSum)
import Options.Applicative

-- withArgs ["-v", "--top", "/mnt/sda2/pCloudSync/Audio/Music/Johann Sebastian Bach"] main

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
  tree <- Map.mapMaybe id <$> getSubdirectoryFilesRecursive verbose (getStatusDeep verbose) keep top
  let mp = makeChecksumMap verbose tree
  reportDuplicates top (keepDuplicates mp)

keep :: Set FileAttribute -> Maybe Sum
keep s = if isRegular s then toSum s else Nothing

reportDuplicates :: FilePath -> Map Sum (Set FilePath) -> IO ()
reportDuplicates top mp = do
  case Map.size mp of
    0 -> putStrLn ("No duplicates found inside " ++ top)
    n -> putStr (show n ++ " sets of duplicates found in " ++ top ++ " -\n" ++ unlines messages)
    where
      messages :: [String]
      messages = fmap dupMessage dups
      dups :: [Set FilePath]
      dups = Map.elems $ Map.filter (\s -> Set.size s > 1) mp
      dupMessage :: Set FilePath -> String
      dupMessage paths = unlines ("duplicates:" : fmap ("  " ++) (Set.toList paths))
