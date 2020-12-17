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

type Cube = (Int, Int, Int)

_x :: Cube -> Int
_x (x,_,_) = x
_y :: Cube -> Int
_y (_,y,_) = y
_z :: Cube -> Int
_z (_,_,z) = z

type Pocket = Set Cube

parseZ :: Int -- Cube dimension index size
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

parseXYZ :: Int -> Int -> Int -> Parser (Maybe Cube)
parseXYZ z y x =
  choice [ char '#' >> pure (Just (x,y,z))
         , char '.' >> pure Nothing ]

printPocketZ :: Pocket -> Int -> IO ()
printPocketZ cube z = do
  let ((lx,ly,_), (ux,uy,_)) = boundsPad cube
  forM_ [ly .. uy] $ \y -> do
    forM_ [lx .. ux] $ \x -> do
      putStr $ if (x,y,z) `Set.member` cube then "#" else "."
    putStrLn ""

neighbours :: Cube -> Pocket
neighbours (x',y',z') = Set.fromDistinctAscList
  [(x' + x, y' + y, z' + z) | x <- [-1..1],
                              y <- [-1..1],
                              z <- [-1..1],
                              (x,y,z) /= (0,0,0)]

-- The bounds of Pocket with padding
boundsPad :: Pocket -> (Cube, Cube)
boundsPad pocket = 
  let planeX = Set.map _x pocket
      planeY = Set.map _y pocket
      planeZ = Set.map _z pocket
  in ((Set.findMin planeX - 1, Set.findMin planeY - 1, Set.findMin planeZ - 1),
      (Set.findMax planeX + 1, Set.findMax planeY + 1 , Set.findMax planeZ + 1))

-- Cube from lower to upper bounds in ascending order
enumCube :: Cube -> Cube -> [Cube]
enumCube (lx,ly,lz) (ux,uy,uz) =
  [(x,y,z) | x <- [lx..ux],
             y <- [ly..uy],
             z <- [lz..uz]]

cycle :: Pocket -> Pocket
cycle pocket =
  let inspect = uncurry enumCube $ boundsPad pocket
  in Set.fromList $ mapMaybe (rule pocket) inspect

rule :: Pocket -> Cube -> Maybe Cube
rule pocket c =
  if c `Set.member` pocket
    then case activeNeighbours pocket c of
      2 -> Just c
      3 -> Just c
      _ -> Nothing
    else case activeNeighbours pocket c of
      3 -> Just c
      _ -> Nothing

activeNeighbours pocket c = Set.size $ Set.intersection pocket (neighbours c)

main :: IO ()
main = do
  Right pocket <- parseOnly (parseZ 7 0) <$> BS.readFile "input"
  let cycles = iterate cycle pocket
  print $ Set.size (cycles !! 6)