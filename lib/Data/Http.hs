module Data.Http 
  ( HttpMethod(..)
  ) where

data HttpMethod
  = POST
  | GET
  | DELETE
  deriving (Show)

