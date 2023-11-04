{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Parsers.UrlParser where

import Parsers.Parser

type UrlParser = Parser [(String, String)]

(//) :: UrlParser -> UrlParser -> UrlParser
(//) pa pb = do
  a <- pa <* char '/'
  b <- pb
  pure $ a <> b

root :: UrlParser
root = char '/' >> pure []

s :: String -> UrlParser
s label = segment label >> pure []

var :: String -> UrlParser
var param = do
  value <- string
  return [(param, value)]
