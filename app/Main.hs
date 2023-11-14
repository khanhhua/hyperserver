{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <&>" #-}
module Main where

import Data.List (singleton)

import Control.Class (Applet)
import Control.HttpApplet (dispatcher, runApplet)
import Control.HttpConnection (serve)
import Control.Monad.Trans.Reader (asks, runReader)
import Data.Response (htmlText, plainText)
import Data.Router (get, post)

import Parsers.UrlParser (root, s, top, var, (//))

import Data.Builder (buildHtmx)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Maybe (fromMaybe)
import Data.Request (Request (params), body)
import qualified Data.Syntax as H
import qualified Data.Tags as H
import Parsers.FormDataParser (formData)
import Parsers.Parser (parse)
import qualified Template.Elements as T

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
      html = runReader (tmplPage [tmplForm items]) []
      content = toStrict $ toLazyByteString (buildHtmx html)
  pure $ htmlText 200 content
 where
  getMulti key =
    reverse . foldl (\acc (name, value) -> if name == key then value : acc else acc) []

  tmplPage body =
    T.tag
      "html"
      []
      [ T.stag
          "head"
          []
          [ H.tag "title" [] [H.text "Hyperserver"]
          , H.script [("src", "https://unpkg.com/htmx.org@1.9.4/dist/htmx.min.js")]
          , H.link [("href", "https://cdn.jsdelivr.net/npm/bootstrap@5.3.2/dist/css/bootstrap.min.css")]
          ]
      , T.tag "body" [] body
      ]
  tmplForm items =
    T.stag
      "form"
      [ ("hx-post", "/todos")
      , ("hx-select", "form")
      , ("hx-swap", "outerHTML")
      , ("hx-trigger", "submit")
      ]
      [ H.tag "h1" [] [H.text "TODO - List"]
      , H.tag
          "ul"
          []
          [H.tag "li" [] [H.tag "input" [("type", "text"), ("name", "todoItem"), ("value", item)] []] | item <- items]
      , H.tag "input" [("type", "text"), ("name", "todoItem")] []
      , H.scTag
          "input"
          [ ("type", "submit")
          ]
      ]
