{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
module Parsers.Parser where

import Control.Applicative (Alternative (empty, (<|>)))
import Control.Monad (void)
import Data.Char (isDigit)
import Data.Maybe (isJust)

newtype Parser a = Parser {runParser :: String -> (Maybe a, String)}

instance Functor Parser where
  fmap f (Parser p) =
    Parser
      ( \xs ->
          let (ma, xs') = p xs
           in (f <$> ma, xs')
      )

instance Applicative Parser where
  pure a = Parser (\xs -> (Just a, xs))

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) (Parser f) (Parser p) =
    Parser
      ( \xs ->
          let
            (mf, xs') = f xs
            (ma, xs'') = p xs'
           in
            case mf of
              Nothing -> (Nothing, xs)
              Just f' ->
                case ma of
                  Nothing -> (Nothing, xs)
                  Just a -> (Just $ f' a, xs'')
      )
  (<*) pa pb = do
    a <- pa
    _ <- pb
    pure a

instance Monad Parser where
  return = pure
  (Parser p) >>= f =
    Parser
      ( \xs ->
          let (ma, xs') = p xs
           in case ma of
                Nothing -> (Nothing, xs)
                Just a ->
                  let (Parser f') = f a
                   in f' xs'
      )

instance Alternative Parser where
  empty = Parser (\xs -> (Nothing, xs))
  (<|>) (Parser pa) (Parser pb) =
    Parser
      ( \xs ->
          let (ma, xsa) = pa xs
           in if isJust ma
                then (ma, xsa)
                else
                  let (mb, xsb) = pb xs
                   in if isJust mb
                        then (mb, xsb)
                        else (Nothing, xs)
      )

parsePrefix :: Parser a -> String -> Maybe a
parsePrefix p = fst . runParser p

parse :: Parser a -> String -> Maybe a
parse p s =
  case runParser p s of
    (x, []) -> x
    _ -> Nothing

predicate :: (Char -> Bool) -> Parser Char
predicate test =
  Parser
    ( \s ->
        case s of
          [x] -> if test x then (Just x, "") else (Nothing, s)
          (x : xs) -> if test x then (Just x, xs) else (Nothing, s)
          _ -> (Nothing, "")
    )

letter :: Parser Char
letter = predicate $ const True

digit :: Parser Char
digit = predicate isDigit

alphanumeric :: Parser Char
alphanumeric = predicate (/= '/')

char :: Char -> Parser Char
char c = predicate (== c)

segment_ :: String -> Parser String
segment_ "" = error "Parser error"
segment_ s@(x : xs)
  | length s == 1 = char x >> pure s
  | otherwise = char x >> segment xs >> pure s

segment :: String -> Parser ()
segment s = void (segment_ s)

int :: Parser String
int = many digit

string :: Parser String
string = many alphanumeric

many :: Parser Char -> Parser String
many m =
  phrase ""
 where
  phrase acc =
    (m >>= (\d -> phrase $ d : acc)) <|> (pure . reverse) acc
