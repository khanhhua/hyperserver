module Template.Class (TemplateX, AttributesX) where

import Control.Monad.Trans.Reader (Reader)
import Data.Syntax (AttrList, Html)

type TemplateX a b = Reader a (Html b)
type AttributesX a b = Reader a (AttrList b)
