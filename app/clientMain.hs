{-# LANGUAGE OverloadedStrings #-}
module Main where
import           ClassyPrelude
import           Client
main :: IO ()

main = do
    run  "http://localhost:8080/api"
