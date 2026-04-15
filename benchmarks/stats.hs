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

data Welford = Welford
  { mean  :: Double
  , count :: Int
  , m2    :: Double
  }
  deriving Show

main :: IO ()
main = do
  dirs <- listDirectory "data"
  let last2 = (map ("data" </>) . take 2 . reverse . sort) dirs
  putStr $ "Comparing:\n" ++ unlines last2
  files <- traverse listDirectory last2
  let common = Set.toAscList . Set.unions $ map Set.fromList files
  putStr $ "Common files:\n" ++ (unlines common)
  let [d2,d1] = last2
  let pairs = zip (map (d1 </>) common) (map (d2 </>) common)
  putStrLn $ "Pairs:\n" ++ show pairs


stats :: String -> String
stats = summarise . map parse . map words . lines

parse :: [String] -> (Code, Measure)
parse [c,m] = (read c, read m)

summarise :: [(Code, Measure)] -> String
summarise = render . foldr summarise' (0, Welford 0 0 0)

summarise' :: (Code, Measure) -> (Int, Welford) -> (Int, Welford)
summarise' (code, x) (e, w)
  | code == 200 = (e, welford x w)
  | otherwise   = (e+1, w)

render :: (Int, Welford) -> String
render (e, w) = unlines $
  [unwords $ map show [e, count w] ++ map show [mean w, stdDev w]]

stdDev :: Welford -> StdDev
stdDev w = sqrt (m2 w / (fromIntegral ((count w) - 1)))

welford :: Measure -> Welford -> Welford
welford x w = Welford
  { mean = mean'
  , count = count'
  , m2 = m2 w + (x - mean w) * (x - mean')
  }
  where
    mean' = mean w + (x - mean w)/(fromIntegral count')
    count' = count w + 1
