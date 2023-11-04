module Main where

import Control.HttpApplet (Applet, dispatcher, runApplet)
import Control.HttpConnection (serve)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (asks)
import Data.Http (HttpMethod (..))
import Data.Request (Request (params))
import Data.Response (plainText)
import Parsers.UrlParser (root, s, var, (//))

main :: IO ()
main = serve Nothing "8080" $ runApplet $ dispatcher routingTable
 where
  routingTable =
    [ get $ root home
    , get $ root // s "about" about
    , get $ root // s "pages" // var ":id" pageN
    ]

home :: Applet
home = do
  pure $ plainText 200 "It works"
about :: Applet
about = do
  pure $ plainText 200 "Written in Haskell with Fist"

pageN :: Applet
pageN = do
  ps <- asks params
  liftIO $ print ps
  pageId <- asks (lookup ":id" . params)
  pure $ plainText 200 $ "The page is #" <> show pageId
