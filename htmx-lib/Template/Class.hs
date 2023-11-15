module Template.Class (TemplateX, AttributesX) where

import Control.Monad.Trans.Reader (Reader)
import Data.Syntax (AttrList, Htmx)

-- type Template a = Reader a (Htmx Value)
-- type Attributes a = Reader a (AttrList Value)

type TemplateX a b = Reader a (Htmx b)
type AttributesX a b = Reader a (AttrList b)

{-|
<*> :: AttributesX a (b -> c) -> AttributesX a b -> AttributesX a c
-}
