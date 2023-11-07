module Control.HttpApplet where

import Control.Monad.IO.Class (liftIO)
import Data.Request
import Data.Response
import Parsers.Parser (parse)

import Control.Class (Applet)
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask, asks, local)
import Data.Router (Routing)

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
