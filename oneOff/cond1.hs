#!/usr/bin/env stack
-- stack script --resolver lts-17.0

import ClassyPrelude
import Conduit

main :: IO ()
main =
  runConduit $
    yieldMany [1 .. 10]
      .| iterMC print
      .| liftIO (putStrLn "I was called")
      .| sinkNull
