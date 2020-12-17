{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Prelude hiding ( cycle )

import Control.Monad ( forM_ )
import Data.Attoparsec.ByteString.Char8 ( Parser, parseOnly, char, choice, endOfLine, endOfInput)
import Data.ByteString ( ByteString )
import qualified Data.ByteString as BS
import Data.Maybe ( catMaybes, mapMaybe )
import Data.Set ( Set )
import qualified Data.Set as Set

type Hyper = (Int, Int, Int, Int)

_x :: Hyper -> Int
_x (x,_,_,_) = x
_y :: Hyper -> Int
_y (_,y,_,_) = y
_z :: Hyper -> Int
_z (_,_,z,_) = z
_h :: Hyper -> Int
_h (_,_,_,h) = h

type Pocket = Set Hyper


-- The parser is unchanged from Part 1 except
-- h is set to 0.
parseZ :: Int -- Hyper dimension index size
       -> Int -- Z plane
       -> Parser Pocket
parseZ w z = do
  ys <- mapM (parseYZ w z) [0..w]
  endOfInput
  pure $ Set.unions ys

parseYZ :: Int -> Int -> Int -> Parser Pocket
parseYZ w z y = do
  xs <- mapM (parseXYZ z y) [0..w]
  endOfLine 
  pure . Set.fromList $ catMaybes xs

parseXYZ :: Int -> Int -> Int -> Parser (Maybe Hyper)
parseXYZ z y x =
  choice [ char '#' >> pure (Just (x,y,z,0))
         , char '.' >> pure Nothing ]

printPocketZ :: Pocket -> Int -> Int -> IO ()
printPocketZ pocket z h = do
  let ((lx,ly,_,_), (ux,uy,_,_)) = boundsPad pocket
  forM_ [ly .. uy] $ \y -> do
    forM_ [lx .. ux] $ \x -> do
      putStr $ if (x,y,z,h) `Set.member` pocket then "#" else "."
    putStrLn ""

neighbours :: Hyper -> Pocket
neighbours (x',y',z',h') = Set.fromDistinctAscList
  [(x'+x,y'+y,z'+z,h'+h) | x <- [-1..1],
                           y <- [-1..1],
                           z <- [-1..1],
                           h <- [-1..1],
                           (x,y,z,h) /= (0,0,0,0)]

-- The bounds of Pocket with padding
boundsPad :: Pocket -> (Hyper, Hyper)
boundsPad pocket = 
  let planeX = Set.map _x pocket
      planeY = Set.map _y pocket
      planeZ = Set.map _z pocket
      planeH = Set.map _h pocket
  in ((Set.findMin planeX - 1, Set.findMin planeY - 1, Set.findMin planeZ - 1, Set.findMin planeH - 1),
      (Set.findMax planeX + 1, Set.findMax planeY + 1 , Set.findMax planeZ + 1, Set.findMax planeH + 1))

-- Hyper from lower to upper bounds in ascending order
enumHyper :: Hyper -> Hyper -> [Hyper]
enumHyper (lx,ly,lz,lh) (ux,uy,uz,uh) =
  [(x,y,z,h) | x <- [lx..ux],
               y <- [ly..uy],
               z <- [lz..uz],
               h <- [lh..uh]]

cycle :: Pocket -> Pocket
cycle pocket =
  let inspect = uncurry enumHyper $ boundsPad pocket
  in Set.fromList $ mapMaybe (rule pocket) inspect

rule :: Pocket -> Hyper -> Maybe Hyper
rule pocket c =
  if c `Set.member` pocket
    then case activeNeighbours pocket c of
      2 -> Just c
      3 -> Just c
      _ -> Nothing
    else case activeNeighbours pocket c of
      3 -> Just c
      _ -> Nothing

activeNeighbours pocket c = Set.size $ Set.intersection (neighbours c) pocket

main :: IO ()
main = do
  Right pocket <- parseOnly (parseZ 7 0) <$> BS.readFile "input"
  let cycles = iterate cycle pocket
  print $ Set.size (cycles !! 6)
  
