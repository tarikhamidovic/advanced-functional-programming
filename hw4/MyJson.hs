module MyJson where

import MyParser
import Control.Applicative
import Data.Char


data JSON = JSONNull 
          | JSONBool
          | JSONString
          | JSONNumber
          | JSONArray [JSON]
          | JSONObj [(String, JSON)]
          deriving (Show, Eq)

spaceOrNewLine :: Parser String
spaceOrNewLine = munchSpacesP <|> stringP "\n"

integerP :: Parser String
integerP = (++) <$> optMinus <*> munch1P isDigit
  where
    optMinus = optionP "" (stringP "-")

jsonNullP :: Parser JSON
jsonNullP = JSONNull <$ stringP "null"

jsonBoolP :: Parser JSON
jsonBoolP = JSONBool <$ (stringP "true" <|> stringP "false")

keyP :: Parser String 
keyP = charP '\"' *> munchP (/= '\"') <* charP '\"'

jsonStringP :: Parser JSON 
jsonStringP = JSONString <$ keyP

jsonNumberP :: Parser JSON
jsonNumberP = JSONNumber <$ choiceP [v1, integerP]
  where
    v1 = (\p1 p2 p3 -> p1 ++ p2 ++ p3) <$> integerP <*> stringP "." <*> munch1P isDigit

jsonValuesP :: Parser JSON
jsonValuesP = choiceP [jsonNullP, jsonBoolP, jsonStringP, jsonNumberP, jsonArrayP, jsonObjP]

jsonArrayP :: Parser JSON
jsonArrayP = JSONArray <$> (charP '[' *> munchSpacesP *> sepByP jsonValuesP (munchSpacesP *> charP ',' <* munchSpacesP) <* munchSpacesP <* charP ']')

jsonPairP :: Parser (String, JSON)
jsonPairP = (,) <$> (keyP <* munchSpacesP <* charP ':' <* munchSpacesP) <*> jsonValuesP

jsonObjP :: Parser JSON
jsonObjP = JSONObj <$> (spaceOrNewLine *> charP '{' *> spaceOrNewLine *> pairs <* spaceOrNewLine <* charP '}' <* spaceOrNewLine)
          where pairs = sepByP jsonPairP (munchSpacesP *> charP ',' <* munchSpacesP)

jsonP :: Parser JSON
jsonP = jsonObjP