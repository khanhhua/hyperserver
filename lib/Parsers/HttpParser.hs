module Parsers.HttpParser (httpHeaderParser) where

import Control.Applicative ((<|>))
import Data.Char (isAscii, isSpace)
import Data.Http (HttpMethod, Version)
import Data.List (singleton)
import Data.Url (Path)
import Parsers.Parser (Parser, char, many, predicate)

type RequestProlog = (HttpMethod, Path, Version)

type RequestHeader = (RequestProlog, [(String, String)])

httpHeaderParser :: Parser RequestHeader
httpHeaderParser = do
  prolog <- prologParser
  headers <- headersParser []
  pure (prolog, headers)
 where
  prologParser = do
    method <- read <$> string :: Parser HttpMethod
    path <- space >> string <* space
    version <- string <* crln
    pure (method, path, version)

  headersParser acc = do
    field <- (headerField <* sep) <|> (singleton <$> crln)
    if field == "\n"
      then pure acc
      else do
        value <- headerValue <* crln
        headersParser ((field, value) : acc)

string :: Parser String
string = many $ predicate (not . isSpace)
space :: Parser Char
space = char ' '
crln :: Parser Char
crln = char '\r' >> char '\n'
sep :: Parser Char
sep = char ':' >> char ' '

headerField :: Parser String
headerField = many $ predicate (\c -> isAscii c && c /= ':')
headerValue :: Parser String
headerValue = many $ predicate (\c -> isAscii c && c /= '\r' && c /= '\n')
