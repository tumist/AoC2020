{-# LANGUAGE  OverloadedStrings #-}
module Main where

import Control.Monad ( forM )
import qualified Data.Attoparsec.ByteString.Char8 as AP
import Data.ByteString ( ByteString )
import qualified Data.ByteString.Char8 as C8

data Math
  = Number Int
  | Plus Math Math
  | Mult Math Math
  deriving (Eq)

instance Show Math where
  show (Number n) = show n
  show (Plus l r) = "(" ++ show l ++ " + " ++ show r ++ ")"
  show (Mult l r) = "(" ++ show l ++ " * " ++ show r ++ ")"

eval :: Math -> Int
eval (Number n) = n
eval (Plus l r) = eval l + eval r
eval (Mult l r) = eval l * eval r

parseSingle :: AP.Parser Math
parseSingle = AP.choice [ parseNumber, parseParens ]

parseNumber :: AP.Parser Math
parseNumber = Number <$> AP.decimal

parseParens :: AP.Parser Math
parseParens = do
  AP.char '('
  middle <- parseMath Nothing
  AP.char ')'
  pure middle

parseOperator :: AP.Parser (Math -> Math -> Math)
parseOperator = AP.choice [ AP.char '+' >> pure Plus
                          , AP.char '*' >> pure Mult ]

-- Left-precedence parser
-- Takes in whatever is to the left of operator that is being parsed,
-- so run it with "parseMath Nothing" if the first input is not an operator.
parseMath :: Maybe Math
          -> AP.Parser Math
parseMath mleft = AP.choice [ operator mleft, end mleft ]
  where
    operator mleft = do
      left <- maybe parseSingle pure mleft
      AP.skipSpace 
      op <- parseOperator
      AP.skipSpace 
      right <- parseSingle
      parseMath . Just $ left `op` right
    end Nothing = parseSingle
    end (Just left) = pure left

-- Part 2: Advanced Math!
parseSingleAdvanced :: AP.Parser Math
parseSingleAdvanced = AP.choice [ parseNumber, parseParensAdvanced ]

parseParensAdvanced :: AP.Parser Math
parseParensAdvanced = do
  AP.char '('
  middle <- parseMathAdvanced Nothing
  AP.char ')'
  pure middle

parseMathAdvanced :: Maybe Math -> AP.Parser Math
parseMathAdvanced mleft = AP.choice [ operator mleft, end mleft ]
  where
    operator mleft = do
      left <- maybe parseSingleAdvanced pure mleft
      AP.skipSpace 
      opChar <- AP.choice [AP.char '+', AP.char '*']
      AP.skipSpace
      case opChar of
        '+' -> do
          -- Left-hand precedence as in parseMath
          right <- parseSingleAdvanced
          parseMathAdvanced . Just $ left `Plus` right
        '*' -> do
          -- Right-hand precedence
          rest <- parseMathAdvanced Nothing
          pure $ left `Mult` rest
    end Nothing = parseSingleAdvanced
    end (Just left) = pure left

getInput :: FilePath -> AP.Parser Math -> IO [Math]
getInput fp parser = do
  lines <- C8.lines <$> C8.readFile fp
  forM lines $ \line -> do
    case AP.parseOnly parser line of
      Right math -> return math
      Left s -> fail $ "Parse failed " ++ show s ++ ": " ++ C8.unpack line

main :: IO ()
main = do
  maths <- getInput "input" (parseMath Nothing)
  print $ sum $ map eval maths
  advanced <- getInput "input" (parseMathAdvanced Nothing)
  print $ sum $ map eval advanced
