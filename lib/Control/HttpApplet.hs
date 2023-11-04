module Control.HttpApplet where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (Reader, ReaderT, ask, asks, local, runReaderT)

import Data.Http (HttpMethod)
import Data.Request
import Data.Response
import Parsers.Parser (parse)
import Parsers.UrlParser (UrlParser)

type Applet = ReaderT Request IO Response

newtype Handler = Handler {route :: (HttpMethod, UrlParser) -> Applet}
type Routing = (HttpMethod, UrlParser, Applet)

defaultApplet :: Applet
defaultApplet = do
  request <- ask

  liftIO $ do
    putStrLn $ "[REQ] " <> show (method request)
    pure $ plainText 404 "Not Found"

dispatcher :: [Routing] -> Applet
dispatcher [] = defaultApplet
dispatcher ((m, p, applet) : xs) = do
  (httpMethod, httpPath) <- asks (\req -> (method req, path req))

  if httpMethod /= m
    then dispatcher xs
    else do
      let mparams = parse p httpPath
      case mparams of
        Nothing -> dispatcher xs
        Just params ->
          local (\req -> req{params = params}) applet

-- TODO middleware as (param... -> Request -> Request) transformation

runApplet :: Applet -> Request -> IO Response
runApplet = runReaderT
