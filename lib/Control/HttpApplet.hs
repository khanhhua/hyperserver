module Control.HttpApplet where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)

import Data.Request
import Data.Response

type Applet = ReaderT Request IO Response

defaultApplet :: Applet
defaultApplet = do
  request <- ask

  liftIO $ do
    putStrLn $ "[REQ] " <> show (method request)
    pure $ plainText 200 "OK"

runApplet :: Applet -> Request -> IO Response
runApplet = runReaderT
