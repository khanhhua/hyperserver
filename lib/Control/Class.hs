module Control.Class where

import Control.Monad.Trans.Reader (ReaderT)
import Data.Request (Request)
import Data.Response (Response)

type Applet = ReaderT Request IO Response
