module Main where

import ExternalInterfaces.ApplicationAssembly

main :: IO ()
main = loadConfig >>= runApp
