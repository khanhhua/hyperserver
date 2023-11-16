module Components.QuickSearch where

import Data.Syntax (AttrList (AttrList), Html, Value, attr)
import qualified Data.Tags as H

type Keyword = String
type Results = [String]

data QSModel = Model Keyword Results
data QSAttr
  = Delay Int
  | Rebounce Int

quicksearch :: QSModel -> Html Value
quicksearch (Model keyword results) =
  H.div
    (AttrList [])
    [ H.input (AttrList [attr "value" keyword])
    , H.ul
        (AttrList [])
        [H.li (AttrList []) [H.text item] | item <- results]
    ]
