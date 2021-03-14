#!/usr/bin/env stack
-- stack script --resolver lts-17.0

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Monad

-- ....
start :: IO ()
start = do
  putStrLn "Before the loop!"
  -- we define "loop" as a recursive IO action
  let loop = do
        putStrLn "Hello, what is your name?"
        name <- getLine
        putStrLn $
          "Welcome to our personality test " ++ name
            ++ ", inspired by the Big Five Theory."
        putStrLn "You will receive fifty questions in total to which you can reply with Yes or No."
        putStrLn "Whenever you feel ready to begin please write Start"
        goGlenn <- getLine
        putStrLn goGlenn
        -- if we did not finish, start another loop
        when (goGlenn /= "start") loop
  loop -- start the first iteration
  putStrLn "After the loop!"

main :: IO ()
main = start
