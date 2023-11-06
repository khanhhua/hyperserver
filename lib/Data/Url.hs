module Data.Url where

import Data.ByteString.Builder (Builder, stringUtf8)
import Data.Maybe (isNothing, fromJust)

data Url
  = Absolute String
  | Relative Path (Maybe Query)

type Path = String

type Query = String

type Seqment = String

urlUtf8 :: Url -> Builder
urlUtf8 (Absolute s) = stringUtf8 s
urlUtf8 (Relative path mquery) = 
  stringUtf8 path
    <> (if isNothing mquery
         then mempty
         else
           stringUtf8 "?"
           <> stringUtf8 (fromJust mquery)
      )
