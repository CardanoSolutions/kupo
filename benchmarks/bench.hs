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

import Control.Applicative (ZipList (..))
import Data.Map (Map)
import qualified Data.Map as Map
import System.Directory
import System.FilePath
import System.IO

type Measure = Double
type StdDev  = Double

type Benchmarks = Map String [[Measure]]

data Welford = Welford
  { mean  :: Double
  , count :: Int
  , m2    :: Double
  }
  deriving Show

main :: IO ()
main =
  do
    fps <- listDirectory "data"
    benchmarks <- loop Map.empty fps
    let summary = Map.map summarise benchmarks
    putStrLn (show summary)

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
    metrics = read <$> tail ss
    collect Nothing   = Just [metrics]
    collect (Just ms) = Just (metrics:ms)

summarise :: [[Measure]] -> [StdDev]
summarise = fmap stdDev . getZipList . foldr welford' (pure (Welford 0 0 0)) . map ZipList
  where
    welford' zm zws = pure welford <*> zm <*> zws

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

{- OUTPUT
fromList
[("*?spent_after=98245654&spent_before=98245660"
 ,[0.0
  ,0.17902759983889716
  ,0.17899436123835366
  ,5.772843030011725e-4
  ,2.2387422259110404e-2
  ,99408.80492649718
  ,487533.0924850821
  ]
 )
,("*@bc40cc86ed43d84d3367a7ff2f4a401dbaed885af96edf1c8fd7379402735699"
 ,[0.0
  ,9.430419625370324e-3
  ,1.1219360994471126e-2
  ,3.796164259090375e-4
  ,4.894611335426145e-4
  ,446.09163240014135
  ,631.5273795028523
  ]
 )
,("a9fc2c980e6beed499b91089ca06ad433961a6238690219b8021fe43.*"
 ,[0.0
  ,0.2059121165980127
  ,0.1226204471770456
  ,5.304550060419035e-3
  ,4.4258886314093866e-2
  ,1115.9960644545044
  ,5727.121133736812
  ]
 )
,("a9fc2c980e6beed499b91089ca06ad433961a6238690219b8021fe43.*?created_before=98245654&spent_after=98764054"
 ,[0.0
  ,3.7576027711956964e-2
  ,5.10052117556867e-2
  ,3.404839121567331e-2
  ,7.058639898720125e-3
  ,1876.1221243529912
  ,7563.922622398647
 ]
 )
,("addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw"
 ,[0.0
  ,0.1491694888753945
  ,0.14510408146074316
  ,3.422209431891247e-2
  ,3.301663127717384e-2
  ,85430.16486183144
  ,452051.9457919027
  ]
 )
,("addr_test1vzpwq95z3xyum8vqndgdd9mdnmafh3djcxnc6jemlgdmswcve6tkw?created_after=98677654&created_before=98764054"
 ,[0.0
  ,3.930792245975458e-2
  ,2.0729466119713155e-2
  ,6.780288955840263e-3
  ,6.776616719503247e-3
  ,122593.1096276753
  ,547948.0453795412
  ]
 )
,("stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r"
 ,[0.0
  ,8.631090551561993e-3
  ,1.1633161664345276e-2
  ,4.359127878830683e-4
  ,1.1054751887107645e-3
  ,422.5229375992391
  ,619.7404590015054
  ]
 )
,("stake_test1upyfx7klyd6lapdyqa0ku2ycgpnz9l8lmvp2ej989l6a69c0vnz0r?spent_after=98245654"
 ,[0.0
  ,7.386877563905189e-3
  ,8.915860973597195e-3
  ,2.6122811173026346e-4
  ,4.359411987816578e-4
  ,721.7057987388818
  ,844.6686389719075
  ]
 )
]
-}
