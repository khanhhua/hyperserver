module Template.ElementX where

import Control.Monad (liftM2)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.Reader (ask)

import qualified Data.Syntax as H
import Template.Class (TemplateX, AttributesX)
import Data.Syntax (AttrList(AttrList), Htmx (TextNode))

type TemplateXElement a = TemplateX [(String, a)] a
type TemplateXAttributes a = AttributesX [(String, a)] a

tag :: H.TagName -> TemplateXAttributes a -> [TemplateXElement a] -> TemplateXElement a
tag tagName attrsX childrenX = do
  -- attrs <- attrsX
  -- children <- sequence childrenX
  -- stag tagName attrs children
  liftM2 (H.tag tagName) attrsX (sequence childrenX)

stag :: H.TagName -> AttrList a -> [H.Htmx a] -> TemplateXElement a
stag tagName attrs children =
  pure $ H.tag tagName attrs children

text :: a -> TemplateXElement a
text = pure . TextNode

scTag :: H.TagName -> TemplateXAttributes a -> TemplateXElement a
scTag tagName attrsX =
  H.scTag tagName <$> attrsX

attributes :: [(String, String)] -> TemplateXAttributes a
attributes table = do
  env <- ask
  let mattrs = traverse (\(k, v) -> H.attr k <$> lookup v env) table
   in pure . AttrList $ fromMaybe [] mattrs

sattributes :: [(String, a)] -> TemplateXAttributes a
sattributes table =
  let attrs = uncurry H.attr <$> table
   in pure $ AttrList attrs
