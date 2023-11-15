{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Data.Tags where

import Data.Syntax

html :: [Htmx Value] -> Htmx Value
html = tag "html" (AttrList [])

head = tag "head" (AttrList [])
body = tag "body"
title = tag "title"
script attrs = tag "script" attrs []
link = scTag "link"

h1 = tag "h1"
h2 = tag "h2"
h3 = tag "h3"
h4 = tag "h4"
h5 = tag "h5"
h6 = tag "h6"

div = tag "div"

form = tag "form"
ul = tag "ul"
li = tag "li"
para = tag "p"
anchor = tag "a"

img = scTag "img"

input = scTag "input"

label = tag "label"

text = TextNode
