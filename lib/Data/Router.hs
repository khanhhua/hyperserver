module Data.Router (Routing, get, post, delete) where

import Data.Http (HttpMethod (DELETE, GET, POST))
import Parsers.UrlParser (UrlParser)

import Control.Class (Applet)

type Routing = (HttpMethod, UrlParser, Applet)

get :: UrlParser -> Applet -> Routing
get p a = (GET, p, a)

post :: UrlParser -> Applet -> Routing
post p a = (POST, p, a)

delete :: UrlParser -> Applet -> Routing
delete p a = (DELETE, p, a)
