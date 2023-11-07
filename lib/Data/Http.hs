module Data.Http (
  HttpMethod (..),
  Version,
) where

type Version = String

data HttpMethod
  = POST
  | GET
  | DELETE
  deriving (Show, Eq)

instance Read HttpMethod where
  readsPrec _ = parseS

parseS s = do
  (token, xs) <- lex s
  case token of
    "POST" -> return (POST, xs)
    "GET" -> return (GET, xs)
    "DELETE" -> return (DELETE, xs)
    _ -> fail "Invalid HTTP Verb"
