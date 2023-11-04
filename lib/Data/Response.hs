module Data.Response (
  Response (..),
  plainText,
  toBuilder,
) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder, byteString, intDec, stringUtf8)
import Data.String (IsString (fromString))

data Response = Response
  { status :: Int
  , headers :: [(String, String)]
  , body :: ByteString
  }

toBuilder :: Response -> Builder
toBuilder (Response{..}) =
  stringUtf8 "HTTP/1.1 "
    <> intDec status
    <> stringUtf8 "\n\n"
    <> byteString body

plainText :: Int -> String -> Response
plainText httpStatus content =
  Response
    { status = httpStatus
    , headers = [("Content-Type", "text/plain")]
    , body = fromString content
    }
