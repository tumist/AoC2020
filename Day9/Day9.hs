module Main where

getInput :: FilePath -> IO [Int]
getInput f = lines <$> readFile f >>= return . fmap read

solution :: Int -> [Int] -> Int
solution preLen nums =
  if inspecting `elem` allowedNums
  then solution preLen (drop 1 nums)
  else inspecting
  where
    preamble = take preLen nums
    inspecting = head $ drop preLen nums
    allowedNums = [a + b | a <- preamble, b <- preamble, a /= b]

sumToTarget :: Int -> [Int] -> Maybe [Int]
sumToTarget target list = go 0 [] list
  where
    go accSum acc [] = if accSum == target then Just acc else Nothing
    go accSum acc (x:r) =
      if accSum == target then Just acc
      else if accSum > target then Nothing
           else go (accSum + x) (x:acc) r

findSublist :: Int -> [Int] -> Maybe [Int]
findSublist _      [] = Nothing
findSublist target list =
  case sumToTarget target list of
    Nothing -> findSublist target (tail list)
    Just res -> Just res

solution2 :: Int -> [Int] -> Int
solution2 target list = minimum sublist + maximum sublist
  where Just sublist = findSublist target list

main :: IO ()
main = do
  numbers <- getInput "testInput"
  let result = solution 5 numbers
  print result

  numbers' <- getInput "input"
  let result' = solution 25 numbers'
  print result'
  print $ solution2 result' numbers'
