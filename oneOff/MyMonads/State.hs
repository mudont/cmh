{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

-- https://williamyaoh.com/posts/2020-07-12-deriving-state-monad.html
module MyMonads.State where

import ClassyPrelude

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap :: (a -> b) -> State s a -> State s b
  fmap f (State sa) = State (\s0 -> let (a, s1) = sa s0 in (f a, s1))

instance Applicative (State s) where
  pure :: a -> State s a
  pure a = State (a,)

  (<*>) :: State s (a -> b) -> State s a -> State s b
  (State ra) <*> (State rb) =
    State $ \s0 ->
      let (fn, s1) = ra s0
          (a, s2) = rb s1
       in (fn a, s2)

instance Monad (State s) where
  (>>=) :: State s a -> (a -> State s b) -> State s b
  (State ra) >>= fn = State $ \s0 ->
    let (a, s1) = ra s0
        State rb = fn a
     in rb s1

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ const ((), s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)

eval :: State s a -> s -> a
eval (State sa) s0 = fst $ sa s0

exec :: State s a -> s -> s
exec (State sa) s0 = snd $ sa s0

-------------------------------------------------------------------
-- Use State
-------------------------------------------------------------------

mj :: Int
mj = 23

scottie :: Int
scottie = 33

myStateM :: State Int ()
myStateM = do
  i <- get
  put (i + 1)
  modify $ \s -> s * 2
  return ()

main :: Int
main = exec myStateM 0

-- >>> do putStrLn "GHCI integration worked"
-- (Error while loading modules for evaluation)
-- <BLANKLINE>
-- <no location info>: error: can't find file: MyState.hs
-- Failed, no modules loaded.
--
