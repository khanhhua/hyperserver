module Template.Class (Template, Attributes) where

import Control.Monad.Trans.Reader (Reader)
import Data.Syntax (AttrList, Htmx (..), Value)

type Template a = Reader a (Htmx Value)
type Attributes a = Reader a (AttrList Value)

-- newtype Template a b = Template {render :: a -> Htmx b}
--
-- instance Functor (Template a) where
--  -- fmap :: (b -> c) -> Template b -> Template c
--  fmap f (Template render) =
--    Template
--      ( fmap f . render
--      )
--
-- instance Applicative (Template a) where
--  pure = undefined
--
--  -- (<*>) :: Template (b -> c) -> Template b -> Template c
--  (<*>) (Template f) (Template render) =
--    Template
--      ( \input ->
--            case f input of
--              (TextNode g) ->
--                case render input of
--                  (TextNode out) -> TextNode $ g out
--                  _ -> error "Render failed"
--              (Tag name g h) ->
--                case render input of
--                  (Tag _name attrs children) -> Tag name (g <*> attrs) (h <*> children)
--                  _ -> error "Render failed"
--
--      )
--
-- instance Traversable (Template a) where
