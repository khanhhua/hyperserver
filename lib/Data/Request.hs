module Data.Request (
  Request (..),
) where

import Data.Http (HttpMethod)
import Data.Url (Path)

data Request = Request
  { method :: HttpMethod
  , path :: Path
  , params :: [(String, String)]
  , headers :: [(String, String)]
  , body :: Maybe String
  }
