module Main where

import Control.HttpApplet (defaultApplet, runApplet)
import Control.HttpConnection (serve)

main :: IO ()
main = serve Nothing "8080" $ runApplet defaultApplet
