module Components.ToDoComponent (ToDoModel(..), Keyword, ToDo, todoComponent) where
import qualified Data.Syntax as H
import qualified Data.Tags as H
import Data.Syntax (AttrList(AttrList))

type Keyword = String
type ToDo = String
data ToDoModel = ToDoModel Keyword [ToDo]

todoComponent :: ToDoModel -> H.Htmx H.Value
todoComponent (ToDoModel keyword todos) =
  H.div (AttrList [])
    [ todoInput keyword
    , todoList todos
    ]

todoInput :: Keyword -> H.Htmx H.Value 
todoInput input =
  H.div (AttrList [])
    [ H.input (AttrList [ H.attr "value" input ])
    ]

todoList :: [ToDo] -> H.Htmx H.Value
todoList items =
  H.div (AttrList []) $ do
    item <- items
    pure $ H.div (AttrList []) [ H.input (AttrList [ H.attr "value" item ]) ]
