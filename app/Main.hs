module Main where

import Control.Class (Applet)
import Control.HttpApplet (dispatcher, runApplet)
import Control.HttpConnection (serve)
import Control.Monad.Trans.Reader (asks, runReader)
import Data.Response (htmlText, plainText)
import Data.Router (get, post)

import Parsers.UrlParser (root, s, top, var, (//))

import Components.ToDoComponent (ToDoModel (ToDoModel), todoComponent)
import Data.Builder (buildHtml)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Request (Request (params), body)
import qualified Data.Syntax as H
import qualified Data.Tags as H
import Parsers.FormDataParser (formData)
import Parsers.Parser (parse)
import qualified Template.ElementX as T

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
      mtodoItems = getMulti "todoItem" <$> mform
      items = fromMaybe [] mtodoItems
      html = runReader (tmplPage [tmplToDoComponent $ ToDoModel "" items]) []
      content = toStrict $ toLazyByteString (buildHtml html)
  pure $ htmlText 200 content
 where
  getMulti key =
    reverse . foldl (\acc (name, value) -> if name == key then value : acc else acc) []

  tmplPage body =
    T.tag
      "html"
      (T.sattributes [])
      [ T.stag
          "head"
          (H.attrlist [])
          [ H.tag "title" (H.attrlist []) [H.text "Hyperserver"]
          , H.script (H.attrlist [("src", "https://unpkg.com/htmx.org@1.9.4/dist/htmx.min.js")])
          , H.link (H.attrlist [("href", "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css")])
          ]
      , T.tag "body" (T.sattributes []) body
      ]

  tmplToDoComponent = pure . todoComponent
