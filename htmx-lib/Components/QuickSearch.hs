module Components.QuickSearch (
  QSModel (..),
  QSAttr (..),
  bind,
  quicksearch,
  searchHistory,
) where

import Data.Syntax (AttrList (AttrList), ChildList (ChildList), Html (SCTag, Tag, TextNode), Value)
import qualified Data.Syntax as H
import qualified Data.Tags as H

type Keyword = String
type Results = [String]

data QSModel = Model Keyword Results
data QSAttr
  = Delay Int
  | Rebounce Int

bind :: (QSModel -> Html Value) -> Html QSModel -> Html Value
bind f = \case
  TextNode (Model keyword results) -> renderNode keyword results
  SCTag _ _ -> error "QS Error"
  Tag tagName _ (ChildList children) ->
    let c = bind f <$> children
     in Tag tagName (H.attrlist []) $ ChildList c
 where
  renderNode keyword results =
    H.form
      (H.attrlist [("method", "POST"), ("action", "/search")])
      [ H.input (H.attrlist [("value", keyword), ("name", "keyword")])
      , H.input (H.attrlist [("type", "submit")])
      , H.ul
          (AttrList [])
          [H.li (AttrList []) [H.text item] | item <- results]
      ]

quicksearch :: Keyword -> Results -> Html QSModel
quicksearch keyword results = H.text $ Model keyword results

searchHistory :: [Html QSModel] -> Html QSModel
searchHistory =
  H.tag "search-history" (H.attrlist [])
