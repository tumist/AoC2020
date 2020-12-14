module Main where

import Data.List ( sort, group )

getInput :: FilePath -> IO [Int]
getInput fp = lines <$> readFile fp >>= return . fmap read

countArrangementsNaive :: [Int] -> Int
countArrangementsNaive sortedAdapters = go 0 sortedAdapters
  where
      go curJolt [] = 1
      go curJolt [x] = 1
      go curJolt restAdapters = 
        let usableAdapters = takeWhile (\j -> j <= curJolt + 3) restAdapters
            restArrangements = map (\a -> (a, dropWhile (\j -> j <= a) restAdapters)) usableAdapters
        in sum (map (\(a,rest) -> go a rest) restArrangements)

main :: IO ()
main = do
    adapters <- getInput "input"
    
    let adapterSeries = 0 : sort adapters
    let joltDeltas = map (\(l,r) -> r - l) $ zip adapterSeries (tail adapterSeries)
    --print adapterSeries
    --print joltDeltas

    -- Part 1
    --print $ length $ filter (\j -> j == 1) joltDifference
    --print $  1 + (length $ filter (\j -> j == 3) joltDifference)
    
    -- Part 2
    --print $ countArrangementsNaive (sort adapters)
    print $ product $ map f (group joltDeltas)
    where
      f [1,1] = 2
      f [1,1,1] = 4
      f [1,1,1,1] = 7
      f _ = 1


prime_factors n =
  case factors of
    [] -> [n]
    _  -> factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]