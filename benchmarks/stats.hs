{-
 - Analyse benchmarks produced by oha
 -}

module Stats where

import System.Directory (listDirectory)
import System.FilePath ((</>))
import Data.List (reverse, sort)
import Data.Set (Set)
import qualified Data.Set as Set

type Code    = Int
type Measure = Double
type StdDev  = Double

data RankScore = RankScore
  { s1 :: Float
  , s2 :: Float
  }
  deriving Show

type Rank = (Float,(Measure,Id))
type Ranking = [Rank]

type Id = Int

sample1 :: Id
sample1 = 1

sample2 :: Id
sample2 = 2

type DataSet = [(Code,Measure)]

data Welford = Welford
  { mean  :: Double
  , count :: Int
  , m2    :: Double
  }
  deriving Show

data Analysis = Analysis
  { errors  :: Int
  , welford :: Welford
  , values :: [Measure]
  }
  deriving Show

data Statistics = Statistics
  { analysis1 :: Analysis
  , analysis2 :: Analysis
  , rankScore :: RankScore
  }
  deriving Show

mkAnalysis :: Analysis
mkAnalysis = Analysis 0 (Welford 0 0 0) []

main :: IO ()
main = do
  dirs <- listDirectory "data"
  let last2 = (map ("data" </>) . take 2 . reverse . sort) dirs
  files <- traverse listDirectory last2
  let common = Set.toAscList . Set.unions $ map Set.fromList files
  announce last2 common
  let pairs = [map (</> f) last2 | f <- common]
  results <- sequence $ map analyse pairs
  putStrLn $ unlines $ map report (zip common results)

announce :: [FilePath] -> [FilePath] -> IO ()
announce [dir1,dir2] files = do
  putStrLn "Comparing:"
  putStrLn $ dir1 <> " (s1: experimental group)"
  putStrLn $ dir2 <> " (s2: control group)"
  putStrLn "Common datasets:"
  putStrLn $ unwords files
  putStrLn "Significance level: α = 0.05"

analyse :: [FilePath] -> IO Statistics
analyse paths = do
  files <- traverse readFile paths
  let datasets = map parse files
  pure $ stats $ map analyse' datasets

analyse' :: DataSet -> Analysis
analyse' = foldr summarise mkAnalysis

stats :: [Analysis] -> Statistics
stats as = Statistics a1 a2 $ computeRankScore (map values as)
  where
    [a1,a2] = as

computeRankScore :: [[Measure]] -> RankScore
computeRankScore [v1,v2] =
  score . rank . sort $
    zip (take 100 v1) (repeat sample1) <> zip (take 100 v2) (repeat sample2)

rank :: [(Measure,Id)] -> Ranking
rank = zip [1.0..]

score :: Ranking -> RankScore
score = score' (RankScore 0 0)

score' :: RankScore -> Ranking -> RankScore
score' s (r:(r':rs))
  | v == v'   = score' (tie n n' id id' s) rs
  | otherwise = score' (add n id s) (r':rs)
  where
    (n ,(v ,id )) = r
    (n',(v',id')) = r'
score' s [(n,(_,id))] = add n id s
score' s [] = s

add :: Float -> Id -> RankScore -> RankScore
add x id s
  | id == sample1 = s {s1 = x + (s1 s)}
  | id == sample2 = s {s2 = x + (s2 s)}

tie :: Float -> Float -> Id -> Id -> RankScore -> RankScore
tie n n' id id' s = add avg id $ add avg id' s
  where
    avg = (n+n')/2

report :: (FilePath, Statistics) -> String
report (f, s)
  | (s1 . rankScore) s < 9378 = repErr f s <> faster s
  | (s2 . rankScore) s < 9378 = repErr f s <> slower s
  | otherwise                 = repErr f s <> insignificant

repErr :: FilePath -> Statistics -> String
repErr f s =
  f
  <> ": errors (s1/s2): ("
  <> (show . errors . analysis1) s
  <> "/"
  <> (show . errors . analysis2) s
  <> "); "

insignificant :: String
insignificant = "comparison: not significant"

faster :: Statistics -> String
faster = change "faster"

slower :: Statistics -> String
slower = change "slower"

change :: String -> Statistics -> String
change str stat = "comparison: s1 " <> str <> " by factor " <> ratio stat

ratio :: Statistics -> String
ratio s = show $ mean1/mean2
  where
    mean1 = (mean . welford . analysis1) s
    mean2 = (mean . welford . analysis2) s

parse :: String -> [(Code, Measure)]
parse = map parse' . map words . lines
  where
    parse' [c,m] = (read c, read m)

summarise :: (Code, Measure) -> Analysis -> Analysis
summarise (code, x) a
  | code == 200 = a {welford = addSample x (welford a), values = x:(values a)}
  | otherwise   = a {errors  = errors a + 1}

stdDev :: Welford -> StdDev
stdDev w = sqrt (m2 w / (fromIntegral ((count w) - 1)))

addSample :: Measure -> Welford -> Welford
addSample x w = Welford
  { mean = mean'
  , count = count'
  , m2 = m2 w + (x - mean w) * (x - mean')
  }
  where
    mean' = mean w + (x - mean w)/(fromIntegral count')
    count' = count w + 1
