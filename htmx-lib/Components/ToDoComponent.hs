module Components.ToDoComponent (ToDoModel(..), Keyword, ToDo, todoComponent) where
import qualified Data.Syntax as H
import qualified Data.Tags as H

type Keyword = String
type ToDo = String
data ToDoModel = ToDoModel Keyword [ToDo]

todoComponent :: ToDoModel -> H.Htmx H.Value
todoComponent (ToDoModel keyword todos) =
  H.form
    ( H.attrlist
      [ ("hx-post", "/todos")
      , ("hx-select", "form")
      , ("hx-swap", "outerHTML")
      , ("hx-trigger", "submit")
      ]
    )
    [ todoInput keyword
    , todoList todos
    ]

todoInput :: Keyword -> H.Htmx H.Value 
todoInput input =
  H.div (H.attrlist [])
    [ H.label (H.attrlist []) [ H.text "New Todo" ]
    , H.input (H.attrlist [ ("value", input), ("name", "todoItem") ])
    , H.input (H.attrlist [ ("type", "submit") ])
    ]

todoList :: [ToDo] -> H.Htmx H.Value
todoList items =
  H.div (H.attrlist []) 
    [ H.ul (H.attrlist []) $ do
      item <- items
      pure $ H.div (H.attrlist []) [ H.input (H.attrlist [ ("value", item), ("name", "todoItem") ]) ]
    ]
