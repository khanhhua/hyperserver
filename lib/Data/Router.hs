module Data.Router (Router (..), route) where

import Control.HttpApplet (Applet)
import Data.Http (HttpMethod (DELETE))
import Data.Url (Path)
import Parsers.UrlParser (UrlParser)

type Routing = (HttpMethod, UrlParser, Applet)

get :: UrlParser -> Applet -> Routing
get p a = (GET, p, a)

post :: UrlParser -> Applet -> Routing
post p a = (POST, p, a)

delete :: UrlParser -> Applet -> Routing
delete p a = (DELETE, p, a)
