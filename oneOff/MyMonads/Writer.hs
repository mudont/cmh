{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

-- https://williamyaoh.com/posts/2020-07-26-deriving-writer-monad.html
module MyMonads.Writer where

import ClassyPrelude
  ( Applicative (pure, (<*>)),
    Functor (fmap),
    IO,
    Monad ((>>=)),
    Monoid (mappend, mempty),
    Num ((+)),
    Show (show),
    String,
    print,
    putStrLn,
    ($),
  )

newtype Writer log a = Writer {runWriter :: (log, a)}

-- The instance heads for the Functor/Applicative/Monad trio,
-- since they're a little different for Writer
instance Functor (Writer log) where
  fmap :: (a -> b) -> Writer log a -> Writer log b
  fmap f (Writer (log, a)) = Writer (log, f a)

instance Monoid log => Applicative (Writer log) where
  pure a = Writer (mempty, a)
  Writer (log1, f) <*> Writer (log2, x) = Writer (log1 `mappend` log2, f x)

instance Monoid log => Monad (Writer log) where
  (>>=) :: Writer log a -> (a -> Writer log b) -> Writer log b
  (Writer (log, a)) >>= f = let (Writer (l1, b)) = f a in Writer (log `mappend` l1, b)

tell :: log -> Writer log ()
tell log = Writer (log, ())

censor :: (log -> log) -> Writer log a -> Writer log a
censor fll (Writer (log, a)) = Writer (fll log, a)

listen :: Writer log a -> Writer log (a, log)
listen (Writer (log, a)) = Writer (log, (a, log))

-------------------------------------------------------------------
-- Use Writer
-------------------------------------------------------------------

addTwo :: Num a => a -> Writer [String] a
addTwo x = do
  tell ["Adding 2"]
  pure (x + 2)

augmentAndStringify :: (Show a, Num a) => a -> a -> Writer [String] String
augmentAndStringify x y = do
  tell ["augmenting..."]
  x' <- addTwo x
  y' <- addTwo y
  tell ["stringifying..."]
  pure $ show (x' + y')

main :: IO ()
main = do
  let Writer (_, (_, log)) = listen $ augmentAndStringify 2 3
  print log
  putStrLn "Done"