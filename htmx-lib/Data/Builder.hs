module Data.Builder (buildHtml) where

import Data.ByteString.Builder (Builder, string8)
import Data.Syntax (Attr (Attr), AttrList (AttrList), ChildList (ChildList), Html (SCTag, Tag, TextNode), Value)

lt :: Builder
lt = string8 "<"

sl :: Builder
sl = string8 "/"

gt :: Builder
gt = string8 ">"

sep :: Builder
sep = string8 " "

buildHtml :: Html Value -> Builder
buildHtml (TextNode t) = string8 t
buildHtml (SCTag tagName attrs) =
  lt
    <> bTagName
    <> buildAttrs attrs
    <> sep
    <> sl
    <> gt
 where
  bTagName = string8 tagName
buildHtml (Tag tagName attrs (ChildList children)) =
  lt
    <> bTagName
    <> buildAttrs attrs
    <> gt
    <> childTags
    <> (lt <> sl <> bTagName <> gt)
 where
  bTagName = string8 tagName
  childTags = foldl (<>) mempty $ buildHtml <$> children

buildAttrs :: AttrList Value -> Builder
buildAttrs (AttrList []) = mempty
buildAttrs (AttrList (x : xs)) =
  sep <> buildAttr x <> buildAttrs (AttrList xs)

buildAttr :: Attr Value -> Builder
buildAttr (Attr name value) =
  string8 name <> string8 "=\"" <> string8 value <> string8 "\""
