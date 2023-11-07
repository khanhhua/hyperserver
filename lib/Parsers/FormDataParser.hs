module Parsers.FormDataParser where

import Control.Applicative ((<|>))
import Data.Char (isAscii)
import Parsers.Parser (Parser (..), char, many, predicate, string)

type FormDataParser = Parser [(String, String)]

fieldName :: Parser String
fieldName = many ascii
 where
  ascii = predicate (\c -> isAscii c && c /= '=' && c /= '&')

fieldValue = many ascii
 where
  ascii = predicate (\c -> isAscii c && c /= '&')

field :: FormDataParser
field = do
  name <- fieldName <* char '='
  value <- fieldValue
  pure [(name, value)]

formData :: FormDataParser
formData = do
  collect []
 where
  collect acc = do
    item <- field
    (char '&' >> collect (item <> acc)) <|> pure (reverse $ item <> acc)
