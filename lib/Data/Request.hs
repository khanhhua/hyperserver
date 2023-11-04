module Data.Request
  ( Request(..)
  ) where

import Data.ByteString (ByteString)
import Data.Http (HttpMethod)

data Request = Request
  { method :: HttpMethod
  , headers :: [(String, String)]
  , body :: Maybe ByteString
  }

