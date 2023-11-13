{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs #-}

module Data.Syntax (
  Htmx (..),
  AttrList (..),
  Attr (..),
  ChildList (..),
  TagName,
  Name,
  Value,
  attr,
  tag,
  scTag,
) where

type TagName = String
type Name = String
type Value = String

data Htmx a
  = Tag TagName (AttrList a) (ChildList a)
  | SCTag TagName (AttrList a)
  | TextNode a

instance (Semigroup a) => Semigroup (Htmx a) where
  (<>) (TextNode a) (TextNode b) = TextNode (a <> b)
  (<>) (Tag tagName attrs (ChildList children)) node@(TextNode _) = Tag tagName attrs (ChildList $ children <> [node])
  (<>) (SCTag tagName (AttrList attrsA)) (SCTag _ (AttrList attrsB)) = SCTag tagName (AttrList $ attrsA <> attrsB)
  (<>) (SCTag _ _) _ = error "Syntax fail"
  (<>) a@(Tag _ attrs children) (Tag _ (AttrList []) (ChildList [])) = a
  (<>) (Tag _ (AttrList []) (ChildList [])) a@(Tag tagName attrs children) = a
  (<>) a@(Tag aName _ _) b@(Tag bName _ _)
    | aName == bName = merge a b
    | otherwise = consume a b
   where
    merge (Tag name (AttrList attrsA) (ChildList childrenA)) (Tag _ (AttrList attrsB) (ChildList childrenB)) =
      Tag name (AttrList $ attrsA <> attrsB) (ChildList $ childrenA <> childrenB)
    consume (Tag name attrs (ChildList children)) consumedNode =
      Tag name attrs (ChildList $ children <> [consumedNode])

instance Functor Htmx where
  fmap f (TextNode a) = TextNode (f a)
  fmap f (Tag tagName attrs children) = Tag tagName (f <$> attrs) (f <$> children)
  fmap f (SCTag tagName attrs) = SCTag tagName (f <$> attrs)

instance Applicative Htmx where
  pure = TextNode

  (<*>) (TextNode fx) (TextNode y) =
    TextNode $ fx y
  (<*>) (Tag tagName fx gx) (Tag _ attrs children) =
    Tag tagName newAttrs newChildren
   where
    newAttrs = fx <*> attrs
    newChildren = gx <*> children
  (<*>) (SCTag tagName fx) (SCTag _ attrs) =
    SCTag tagName newAttrs
   where
    newAttrs = fx <*> attrs
  (<*>) _ _ = error "Applicative not fully defined"

instance (Monoid a) => Monoid (Htmx a) where
  mempty = TextNode mempty

-- instance Monad Htmx where
--  return = pure
--  (>>=)

data Attr a = Attr Name a
  deriving (Functor)

instance Applicative Attr where
  pure = Attr "INVALID"
  (<*>) (Attr name fa) (Attr _ a) = Attr name (fa a)

newtype ChildList a = ChildList [Htmx a]
  deriving (Functor)

instance Applicative ChildList where
  pure a = ChildList [TextNode a]
  (<*>) (ChildList fa) (ChildList a) = ChildList $ zipWith (<*>) fa a

newtype AttrList a = AttrList [Attr a]
  deriving (Functor)

instance Applicative AttrList where
  pure a = AttrList [pure a]
  (<*>) (AttrList fa) (AttrList a) =
    AttrList $ zipWith (<*>) fa a

{-
instance Show Attr Value where
  show (Attr name value) = show name ++ "( " ++ show value ++ " )"

instance Show (Htmx Value) where
  show (TextNode t) = show t
  show (Tag name (AttrList []) []) = show $ "<" ++ name ++ " />"
  show (Tag name (AttrList attrs) children) =
    show
      $ "<"
      ++ name
      ++ " attrs: "
      ++ show (length attrsm)
      ++ " children: "
      ++ show (length children)
      ++ " />"
-}

attr :: Name -> Value -> Attr Value
attr = Attr

tag :: TagName -> [(Name, Value)] -> [Htmx Value] -> Htmx Value
tag tagName attrs children = Tag tagName (AttrList $ uncurry attr <$> attrs) (ChildList children)

scTag :: TagName -> [(Name, Value)] -> Htmx Value
scTag tagName attrs = SCTag tagName (AttrList $ uncurry attr <$> attrs)
