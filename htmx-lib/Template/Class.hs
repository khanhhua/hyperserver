module Template.Class (TemplateX, AttributesX, renderHtml) where

import Control.Monad.Trans.Reader (Reader, runReader)
import Data.Syntax (AttrList, Html)

type TemplateX a b = Reader a (Html b)
type AttributesX a b = Reader a (AttrList b)

renderHtml :: Reader r a -> r -> a
renderHtml = runReader
