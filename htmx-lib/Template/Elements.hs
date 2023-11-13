{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use fromMaybe" #-}
module Template.Elements (text, stext, tag, stag, sattributes, div, para, anchor) where

import Prelude hiding (div)

import Control.Applicative (Applicative (liftA2))
import Control.Monad.Trans.Reader (asks)
import Data.Syntax (Attr (Attr), AttrList (AttrList), Name, TagName, Value, attr)
import qualified Data.Syntax as H
import qualified Data.Tags as H
import Template.Class (Attributes, Template)

{-
  tmpl = E.div [("color", "bgColor")]
      [ E.para [("color", "bgColor")]
        [ E.anchor [] "url" "title"
        , E.anchor [("color", "bgColor")] "email" "email"
        ]
      ]

Render by evaluate the Reader against a dictionary:

  x = runReader tmpl [("bgColor", "black"), ("url", "https://goog.le"), ("title", "SEARCH"), ("email", "admin@goog.le")]

Invoke buildHtmx to create a builder

  buildHtmx x

TODO:
  - Introduce syntactical elements for literal and reference
  - Update attributes function to:

     attributes :: [(Name, DynamicAttr)]
  - Rename all HTMX to HTML
  - Introduce new HTMX as a new tree data type for AST
      data Htmy
        = Root
        | LiteralTag Name [(Name, Value)]
        | DynamicTag Name [(Name, DynamicAttr)]
        | DynamicBlock [Htmy]
-}
type TemplateHtmx = Template [(String, String)]

type AttributesHtmx = Attributes [(String, String)]

attributes :: [(Name, String)] -> AttributesHtmx
attributes = collect []
 where
  collect :: [Attr Value] -> [(Name, String)] -> AttributesHtmx
  collect acc [] = pure (AttrList acc)
  collect acc ((key, ref) : xs) = do
    mValue <- asks (lookup ref)
    case mValue of
      Nothing -> collect acc xs
      Just value -> collect (Attr key value : acc) xs

sattributes :: [(Name, Value)] -> AttributesHtmx
sattributes attrs = pure . AttrList $ uncurry attr <$> attrs

anchor :: [(Name, String)] -> String -> String -> TemplateHtmx
anchor mAttrs href title = do
  attrs <- attrsToTuples <$> attributes mAttrs
  mHref <- asks (lookup href)
  mText <- asks (lookup title)
  -- Think of liftA2 as turning function into another with applicative arguments
  pure $ maybe mempty id $ liftA2 (render attrs) mHref mText
 where
  render attrs hrefValue textValue =
    H.anchor (("href", hrefValue) : attrs) [H.text textValue]

stext :: Value -> TemplateHtmx
stext = pure . H.text

text :: String -> TemplateHtmx
text ref = do
  mContent <- asks (lookup ref)
  -- pure :: turns HtmsInstance into TemplateHtmx
  -- maybe default (a -> b) aMaybe :: facilitates default for maybe instances
  pure $ maybe mempty H.text mContent

stag :: TagName -> [(Name, Value)] -> [H.Htmx Value] -> TemplateHtmx
stag tagName attrs children =
  pure $ H.tag tagName attrs children

tag :: TagName -> [(Name, String)] -> [TemplateHtmx] -> TemplateHtmx
tag tagName mAttrs children = do
  attrs <- attrsToTuples <$> attributes mAttrs
  m <- sequenceA children

  pure $ H.tag tagName attrs m

para = tag "p"
div = tag "div"

attrsToTuples :: AttrList Value -> [(Name, Value)]
attrsToTuples (AttrList attrs) = (\(Attr k v) -> (k, v)) <$> attrs
