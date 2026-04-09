{-
 - To begin with this module will analyse 100 benchmark files that have been
 - generated like this:
 -
 - $ mkdir data
 - $ for run in {1..9}; do ./bench data > data/bench-0$run.tsv; done
 - $ for run in {10..99}; do ./bench data > data/bench-$run.tsv; done
 -
 - The objective is to determine the spread of the different variables.
 -}

module Bench where

import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import System.IO

data Metrics = MkMetrics
  { success :: Double
  , total   :: Double
  , slowest :: Double
  , fastest :: Double
  , average :: Double
  , rpsMean :: Double
  , rpsSd   :: Double
  }
  deriving (Eq, Show, Read)

type Benchmarks = Map String [Metrics]

main :: IO ()
main =
  do
    fps <- listDirectory "data"
    benchmarks <- loop Map.empty fps
    putStrLn (show benchmarks)

loop :: Benchmarks -> [FilePath] -> IO Benchmarks
loop b [] = pure b
loop b (fp:fps) =
  do
    f <- readFile ("data" </> fp)
    loop (readBench b f) fps

readBench :: Benchmarks -> String -> Benchmarks
readBench b = foldr (\x b' -> parse b' x) b . map words . lines

parse :: Benchmarks -> [String] -> Benchmarks
parse b ss
  | length ss == 8 = Map.alter collect (ss!!0) b
  | otherwise      = b
  where
    metrics = MkMetrics
      { success = read (ss!!1)
      , total   = read (ss!!2)
      , slowest = read (ss!!3)
      , fastest = read (ss!!4)
      , average = read (ss!!5)
      , rpsMean = read (ss!!6)
      , rpsSd   = read (ss!!7)
      }
    collect Nothing   = Just [metrics]
    collect (Just ms) = Just (metrics:ms)
