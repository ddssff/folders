-- | Find all duplicate regular files in one or more folders.  This
-- is an inherently slow program, it must read the contents of every
-- file to build a checksum.
--
-- Features:
--   (coming) Only read files when there are pairs with equal lengths

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TupleSections #-}
{-# OPTIONS -Wall #-}

import Control.Monad (when)
import Control.Monad.State (evalStateT, MonadState)
import Control.Monad.Trans (liftIO, MonadIO)
import Data.Default (def)
import Data.List (intercalate)
import Data.Map.Strict as Map (delete, elems, filter, lookup, Map, size, unionsWith)
import Data.Set as Set (Set, size, toList)
import Find (FileAttribute, getStatus, getSubdirectoryFilesRecursive, isRegular, keepDuplicates, makeChecksumMap, makeLengthMap, St, Sum, toSum)
import Options.Applicative
import System.Environment (withArgs)

-- withArgs ["-v", "1", "--top", "/mnt/sda2/pCloudSync/Audio/Music/Johann Sebastian Bach"] main

main :: IO ()
main = do
  opts <-
    execParser
      (info
         (options <**> helper)
         (fullDesc
          -- <> progDesc "Print a greeting for TARGET"
          -- <> header "hello - a test for optparse-applicative"
         ))
  evalStateT (go opts) def

data Options = Options [FilePath] Int

options :: Parser Options
options = Options <$> some (strOption ( long "top" <> metavar "FOLDER" <> help "Top folder to search for duplicates"))
                  <*> option auto (long "verbosity" <> short 'v' <> help "Amount of progress messages" <> showDefault <> value 0 <> metavar "INT")

go :: (MonadIO m, MonadState St m) => Options -> m ()
go (Options tops verbosity) = do
  when (verbosity >= 1) (liftIO $ putStrLn $ "Searching for duplicate files in " ++ show tops)
  (trees :: [Map FilePath (Set FileAttribute)]) <- flip evalStateT def $
              mapM (\top -> getSubdirectoryFilesRecursive verbosity (getStatus verbosity) id top) tops
  let tree = (Map.unionsWith (\_ _ -> error "unions") trees :: Map FilePath (Set FileAttribute))
  let lmp = makeLengthMap verbosity tree
  cmp <- makeChecksumMap verbosity tree lmp
  liftIO $ reportDuplicates tops (keepDuplicates cmp)

keep :: Set FileAttribute -> Maybe Sum
keep s = if isRegular s then toSum s else Nothing

reportDuplicates :: [FilePath] -> Map (Maybe Sum) (Set FilePath) -> IO ()
reportDuplicates tops mp0 = do
  case Map.size mp of
    0 -> putStrLn ("No duplicates found inside " ++ show tops)
    n -> putStr (show n ++ " sets of duplicates found in " ++ show tops ++ " -\n" ++ unlines messages)
  maybe (return ()) (\s -> putStrLn (intercalate "\n " ("File whose checksums could not be computed:" : Set.toList s))) nosums
    where
      nosums = Map.lookup Nothing mp0
      mp = Map.delete Nothing mp0
      messages :: [String]
      messages = fmap dupMessage dups
      dups :: [Set FilePath]
      dups = Map.elems $ Map.filter (\s -> Set.size s > 1) mp
      dupMessage :: Set FilePath -> String
      dupMessage paths = unlines ("duplicates:" : fmap ("  " ++) (Set.toList paths))
