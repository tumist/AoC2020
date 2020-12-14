{-# LANGUAGE PartialTypeSignatures #-}
module Main where

import Data.Maybe ( mapMaybe )

data Bus
   = Bus Integer -- Bus ID, Time of departure
         Integer -- Period of departures
         Integer -- Time Offset (bus offset in input list)
         deriving (Show, Eq)

getInput :: FilePath
         -> IO [Bus]
getInput fp = do
  fl <- readFile fp
  let busids "" = []
      busids s = let (digits,rest) = break (== ',') s
                 in digits : busids (if null rest then [] else tail rest)
      busOffset = zip (busids fl) [0..]
  return $ mapMaybe f busOffset
  where
    f ("x",_) = Nothing
    f (digits,offset) = Just $ Bus (read digits) (read digits) offset

reduce :: Bus -> Bus -> Bus
reduce (Bus a pa _) (Bus b pb offset) =
  -- Find a t such that it is divisible by `a`,
  -- stepped by its period, 
  -- and t+offset is divisible by `b`
  let t = head [ t | t <- [a,(a+pa)..], (t + offset) `rem` b == 0]
  in Bus t (pa*pb) 0

main :: IO ()
main = do
  buses <- getInput "input"
  let sol = foldl1 reduce buses
  print sol
  return ()