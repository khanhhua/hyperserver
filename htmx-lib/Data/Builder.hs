module Data.Builder (buildHtmx) where

import Data.ByteString.Builder (Builder, string8)
import Data.Syntax (Attr (Attr), AttrList (AttrList), ChildList (ChildList), Htmx (SCTag, Tag, TextNode), Value)

lt :: Builder
lt = string8 "<"

sl :: Builder
sl = string8 "/"

gt :: Builder
gt = string8 ">"

sep :: Builder
sep = string8 " "

buildHtmx :: Htmx Value -> Builder
buildHtmx (TextNode t) = string8 t
buildHtmx (SCTag tagName attrs) =
  lt
    <> bTagName
    <> buildAttrs attrs
    <> sep
    <> sl
    <> gt
 where
  bTagName = string8 tagName
buildHtmx (Tag tagName attrs (ChildList children)) =
  lt
    <> bTagName
    <> buildAttrs attrs
    <> gt
    <> childTags
    <> (lt <> sl <> bTagName <> gt)
 where
  bTagName = string8 tagName
  childTags = foldl (<>) mempty $ buildHtmx <$> children

buildAttrs :: AttrList Value -> Builder
buildAttrs (AttrList []) = mempty
buildAttrs (AttrList (x : xs)) =
  sep <> buildAttr x <> buildAttrs (AttrList xs)

buildAttr :: Attr Value -> Builder
buildAttr (Attr name value) =
  string8 name <> string8 "=\"" <> string8 value <> string8 "\""
