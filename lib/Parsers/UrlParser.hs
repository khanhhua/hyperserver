{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Parsers.UrlParser where

import Control.Applicative (empty)
import Parsers.Parser

type UrlParser = Parser [(String, String)]

top :: UrlParser
top = char '/' >> pure empty

root :: UrlParser
root = pure []

(//) :: UrlParser -> UrlParser -> UrlParser
(//) pa pb = do
  a <- pa <* char '/'
  b <- pb
  pure $ a <> b

s :: String -> UrlParser
s label = segment label >> pure []

var :: String -> UrlParser
var param = do
  value <- string
  return [(param, value)]
