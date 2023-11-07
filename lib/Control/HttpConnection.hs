{-# LANGUAGE ScopedTypeVariables #-}

module Control.HttpConnection (serve) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import qualified Data.ByteString as B
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import qualified Data.ByteString.Lazy as L
import Data.Http (HttpMethod)
import Data.Request (Request (..))
import Data.Response (Response, toBuilder)
import Network.Socket (
  AddrInfo (addrAddress, addrFlags, addrSocketType),
  AddrInfoFlag (AI_PASSIVE),
  HostName,
  Socket,
  SocketOption (ReuseAddr, ReusePort),
  SocketType (Stream),
  accept,
  bind,
  close,
  defaultHints,
  getAddrInfo,
  gracefulClose,
  listen,
  openSocket,
  setCloseOnExecIfNeeded,
  setSocketOption,
  withFdSocket,
  withSocketsDo,
 )
import Network.Socket.ByteString (sendAll)
import qualified Network.Socket.ByteString.Lazy as N (getContents)

{-
 data HttpConnection = HttpConnection
  { socket :: Socket
  }
 -}
serve :: Maybe HostName -> String -> (Request -> IO Response) -> IO ()
serve mhost port action = withSocketsDo $ do
  addr <- resolve
  putStrLn "Server is listening..."
  E.bracket (open addr) close loop
 where
  resolve = do
    let hints =
          defaultHints
            { addrFlags = [AI_PASSIVE]
            , addrSocketType = Stream
            }
    head <$> getAddrInfo (Just hints) mhost (Just port)
  open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 256
    return sock

  server socket = do
    request <- newRequest socket
    response <- action request
    sendAll socket $ (toStrict . toLazyByteString . toBuilder) response

  loop sock = 
    forever
    $ E.bracketOnError (accept sock) (close . fst)
    $ \(conn, _peer) ->
      void
        $
        -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
        -- but 'E.bracketOnError' above will be necessary if some
        -- non-atomic setups (e.g. spawning a subprocess to handle
        -- @conn@) before proper cleanup of @conn@ is your case
        forkFinally (server conn) (const $ gracefulClose conn 5000)

-- newHttpConnection :: Socket -> HttpConnection
-- newHttpConnection = HttpConnection

newRequest :: Socket -> IO Request
newRequest rawSocket = do
  content <- parts <$> N.getContents rawSocket
  print . head $ content
  let (method, path, _) = httpPrologue . L.toStrict $ head content
  pure $ Request method path [] [] Nothing
 where
  parts xs = L.dropWhile (== 10) <$> L.split 13 xs
  httpPrologue line =
    case unpack <$> B.split 32 line of
      (method : path : version) -> (read method :: HttpMethod, path, version)
      _ -> error "Bad Request"
