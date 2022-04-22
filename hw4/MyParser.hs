{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module MyParser where

import Control.Applicative
import Data.Char
import Data.Functor (($>))

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

parse :: Parser a -> String -> Maybe a
parse p s = fmap snd (runParser p s)

instance Functor Parser where
  fmap f (Parser fp) = Parser $ fmap (f <$>) . fp

instance Applicative Parser where
  pure x = Parser $ \s -> Just (s, x)

  p1 <*> p2 = Parser $ \s -> case runParser p1 s of
    Nothing -> Nothing
    Just (s', f) -> fmap f <$> runParser p2 s'

instance Alternative Parser where
  p1 <|> p2 = Parser $ \s -> runParser p1 s <|> runParser p2 s
  empty = Parser $ const empty

eosP :: Parser ()
eosP = Parser $ \s -> if null s then Just ("", ()) else Nothing

failP :: Parser a
failP = empty

choiceP :: [Parser a] -> Parser a
choiceP = foldl (<|>) empty

optionP :: a -> Parser a -> Parser a
optionP a pa = pa <|> pure a

optionalP :: Parser a -> Parser ()
optionalP p = p $> ()

satP :: (Char -> Bool) -> Parser Char
satP f = Parser $ \case
  (x : xs) -> if f x then Just (xs, x) else Nothing
  _ -> Nothing

charP :: Char -> Parser Char
charP c = satP (== c)

stringP :: String -> Parser String
stringP "" = pure ""
stringP (c : xs) = (:) <$> charP c <*> stringP xs

munchP :: (Char -> Bool) -> Parser String
munchP f = many (satP f)

munch1P :: (Char -> Bool) -> Parser String
munch1P f = some (satP f)

munchSpacesP :: Parser String
munchSpacesP = munchP isSpace

munchSpaces1P :: Parser String
munchSpaces1P = munch1P isSpace

anyOfCharsP :: String -> Parser String
anyOfCharsP s = munch1P (`elem` s)

sepBy1P :: Parser a -> Parser b -> Parser [a]
sepBy1P pa pb = (:) <$> pa <*> some (pb *> pa)

sepByP :: Parser a -> Parser b -> Parser [a]
sepByP pa pb = (:) <$> pa <*> many (pb *> pa)

chainl1P :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1P p op = foldl (\acc (o, y) -> acc `o` y) <$> p <*> p1
  where
    p1 = many ((,) <$> op <*> p)