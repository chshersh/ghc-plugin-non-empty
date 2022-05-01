module Main (main) where

import GhcPluginNonEmpty (projectName)


main :: IO ()
main = putStrLn ("Tests for " ++ projectName)
