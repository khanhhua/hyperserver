module Template.ElementX where

import qualified Data.Syntax as H
import Template.Class (TemplateX, AttributesX)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (fromMaybe)
import Data.Syntax (AttrList(AttrList), Htmx (TextNode))

type TemplateXElement a = TemplateX [(String, a)] a
type TemplateXAttribute a = AttributesX [(String, a)] a

tag :: H.TagName -> TemplateXAttribute a -> [TemplateXElement a] -> TemplateXElement a
tag tagName attrsX childrenX = do
  attrs <- attrsX
  children <- sequence childrenX

  stag tagName attrs children

stag :: H.TagName -> AttrList a -> [H.Htmx a] -> TemplateXElement a
stag tagName attrs children =
  pure $ H.tag tagName attrs children

text :: a -> TemplateXElement a
text = pure . TextNode

scTag :: H.TagName -> TemplateXAttribute a -> TemplateXElement a
scTag tagName attrsX =
  H.scTag tagName <$> attrsX

attributes :: [(String, String)] -> TemplateXAttribute a
attributes table = do
  env <- ask
  let mattrs = traverse (\(k, v) -> H.attr k <$> lookup v env) table
   in pure . AttrList $ fromMaybe [] mattrs

sattributes :: [(String, a)] -> TemplateXAttribute a
sattributes table =
  let attrs = uncurry H.attr <$> table
   in pure $ AttrList attrs
