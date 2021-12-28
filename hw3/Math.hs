{-# OPTIONS_GHC -Wall #-}

module Math where

import Text.ParserCombinators.ReadP
import Data.Char
import Control.Applicative hiding (optional)

data MExp a =
    Num a
  | Add (MExp a) (MExp a)
  | Multiply (MExp a) (MExp a)
  | Subtract (MExp a) (MExp a) deriving (Show)

compute :: (Num a) => MExp a -> a
compute (Num a) = a
compute (Add x y) = compute x + compute y
compute (Multiply x y) = compute x * compute y
compute (Subtract x y) = compute x - compute y

pNum :: String -> Integer
pNum = read 

number :: ReadP (MExp Integer)
number = do
  a <- option "" (string "-")
  b <- munch1 isDigit
  pure $ Num $ pNum (a++b)

parsePlusOrMinus :: ReadP (MExp Integer)
parsePlusOrMinus = do
  a <- term
  op <- char '+' <|> char '-'
  b <- mexpr
  case op of 
    '+' -> pure $ Add a b
    _ -> pure $ Subtract a b

mexpr :: ReadP (MExp Integer)
mexpr = parsePlusOrMinus <|> term 

term :: ReadP (MExp Integer)
term = parseTimes <|> number 

parseTimes :: ReadP (MExp Integer)
parseTimes = do
  a <- number
  _ <- char '*'
  b <- term
  pure $ Multiply a b

parseMExp :: String -> Maybe (MExp Integer)
parseMExp input = case readP_to_S mexpr input of
    [] -> Nothing
    xs -> (Just . fst . last) xs

instance (Num a, Eq a) => Eq (MExp a) where
  x == y = compute x == compute y

instance (Num a, Ord a) => Ord (MExp a) where
  x <= y = compute x <= compute y

instance Num a => Num (MExp a) where
  x + y = Num (compute x + compute y)