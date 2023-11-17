module Components.ToDoComponent (ToDoModel (..), Keyword, ToDo, TemplateXComponent, todoComponent) where

import qualified Data.Syntax as H
import qualified Data.Tags as H
import Template.Class (TemplateX)
import Control.Monad.Trans.Reader (ask)

type Keyword = String
type ToDo = String
data ToDoModel = ToDoModel Keyword [ToDo]

type TemplateXComponent = TemplateX ToDoModel H.Value

-- todoComponent :: ToDoModel -> H.Html H.Value
-- todoComponent (ToDoModel keyword todos) =
todoComponent :: TemplateXComponent
todoComponent = do
  ToDoModel keyword todos <- ask
  pure $ H.form
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

todoInput :: Keyword -> H.Html H.Value
todoInput input =
  H.div
    (H.attrlist [])
    [ H.label (H.attrlist []) [H.text "New Todo"]
    , H.input (H.attrlist [("value", input), ("name", "todoItem")])
    , H.input (H.attrlist [("type", "submit")])
    ]

todoList :: [ToDo] -> H.Html H.Value
todoList items =
  H.div
    (H.attrlist [])
    [ H.ul (H.attrlist []) $ do
        item <- items
        pure $ H.div (H.attrlist []) [H.input (H.attrlist [("value", item), ("name", "todoItem")])]
    ]
