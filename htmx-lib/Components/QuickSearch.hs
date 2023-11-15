module Components.QuickSearch where

import qualified Data.Tags as H
import Data.Syntax (Htmx, Value, AttrList (AttrList), attr)

type Keyword = String
type Results = [String]

data QSModel = Model Keyword Results
data QSAttr
  = Delay Int 
  | Rebounce Int

quicksearch :: QSModel -> Htmx Value
quicksearch (Model keyword results) =
  H.div (AttrList [])
    [ H.input (AttrList [attr "value" keyword])
    , H.ul (AttrList [])
      [ H.li (AttrList []) [ H.text item ] | item <- results]
    ]
