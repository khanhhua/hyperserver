module Main where

import Control.Class (Applet)
import Control.HttpApplet (dispatcher, runApplet)
import Control.HttpConnection (serve)
import Control.Monad.Trans.Reader (asks, withReader)
import Data.Response (htmlText, plainText)
import Data.Router (get, post)

import Parsers.UrlParser (root, s, top, var, (//))

import Components.ToDoComponent (ToDoModel (ToDoModel), todoComponent, TemplateXComponent)
import Data.Builder (buildHtml)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe, fromJust)
import Data.Request (Request (params), body)
import qualified Data.Syntax as H
import qualified Data.Tags as H
import Parsers.FormDataParser (formData)
import Parsers.Parser (parse)
import qualified Template.ElementX as T
import Template.Class (renderHtml)

main :: IO ()
main = serve Nothing "8080" $ runApplet $ dispatcher routingTable
 where
  routingTable =
    [ get top home
    , get (root // s "about") about
    , get (root // s "pages" // var ":id") pageN
    , get (root // s "todos") todoList
    , post (root // s "todos") todoList
    ]

home :: Applet
home = do
  pure $ plainText 200 "It works"
about :: Applet
about = do
  pure $ plainText 200 "Written in Haskell with Fist"

pageN :: Applet
pageN = do
  pageId <- asks (lookup ":id" . params)
  pure $ plainText 200 $ "The page is #" <> show pageId

todoList :: Applet
todoList = do
  mbody <- asks body
  let mform = parse formData mbody
      form = fromMaybe [] mform
      html = renderHtml (tmplPage [todoComponent]) form
      content = toStrict $ toLazyByteString (buildHtml html)
  pure $ htmlText 200 content
 where
  getMulti key =
    reverse . foldl (\acc (name, value) -> if name == key then value : acc else acc) []

  tmplPage :: [TemplateXComponent] -> T.TemplateXElement H.Value
  tmplPage body =
    let seqBody = map (withReader f) body
    in T.tag
      "html"
      (T.sattributes [])
      [ T.stag
          "head"
          (H.attrlist [])
          [ H.tag "title" (H.attrlist []) [H.text "Hyperserver"]
          , H.script (H.attrlist [("src", "https://unpkg.com/htmx.org@1.9.4/dist/htmx.min.js")])
          , H.link (H.attrlist [("href", "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css")])
          ]
      , T.tag "body" (T.sattributes []) seqBody
      ]
    where
      f dict =
        let mkeyword = lookup "keyword" dict
            mtodoItems = getMulti "todoItems" dict
        in ToDoModel (fromJust mkeyword) mtodoItems

  -- tmplToDoComponent :: TemplateXComponent
  -- tmplToDoComponent = asks todoComponent
  -- If the signature of todoComponent were (ToDoModel -> H.Html H.Value)
  --  then (asks todoComponent) :: TemplateX ToDoModel (H.Html H.Value)
  -- Refer to the actual implementation in ToDoComponent.hs
