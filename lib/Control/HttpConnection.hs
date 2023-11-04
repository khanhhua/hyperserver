module Control.HttpConnection (serve) where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, unless, void)
import Data.ByteString.Builder (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Http (HttpMethod (GET))
import Data.Request (Request (..))
import Data.Response (Response, toBuilder)
import Network.Socket (
  AddrInfo (addrAddress, addrFlags, addrSocketType),
  AddrInfoFlag (AI_PASSIVE),
  HostName,
  Socket,
  SocketOption (ReuseAddr),
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

{-
 data HttpConnection = HttpConnection
  { socket :: Socket
  }
 -}
serve :: Maybe HostName -> String -> (Request -> IO Response) -> IO ()
serve mhost port action = withSocketsDo $ do
  addr <- resolve
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
    withFdSocket sock setCloseOnExecIfNeeded
    bind sock $ addrAddress addr
    listen sock 1024
    return sock

  server socket = do
    request <- newRequest socket
    response <- action request
    sendAll socket $ (toStrict . toLazyByteString . toBuilder) response

  loop sock = forever
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
newRequest rawSocket = pure $ Request GET [] Nothing
