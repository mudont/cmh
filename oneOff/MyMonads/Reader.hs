-- https://williamyaoh.com/posts/2020-07-19-deriving-reader-monad.html
{-# LANGUAGE InstanceSigs #-}

-- {-# LANGUAGE TupleSections #-}
-- import Control.Monad.Reader
module MyMonads.Reader where

import ClassyPrelude hiding (Reader, asks)

newtype Reader cfg a = Reader {runReader :: cfg -> a}

instance Functor (Reader cfg) where
  fmap :: (a -> b) -> Reader cfg a -> Reader cfg b
  fmap f (Reader ra) = Reader $ f . ra

instance Applicative (Reader cfg) where
  pure :: a -> Reader cfg a
  pure a = Reader (const a)

  (<*>) :: Reader cfg (a -> b) -> Reader cfg a -> Reader cfg b
  Reader cfgF <*> Reader cfgX = Reader $ \cfg -> cfgF cfg (cfgX cfg)

instance Monad (Reader cfg) where
  (>>=) :: Reader cfg a -> (a -> Reader cfg b) -> Reader cfg b
  Reader cfgA >>= fAB = Reader $ \cfg -> runReader (fAB (cfgA cfg)) cfg

ask :: Reader cfg cfg
ask = Reader id

asks :: (cfg -> a) -> Reader cfg a
asks = Reader

-------------------------------------------------------------------
-- Use Reader
-------------------------------------------------------------------

hello :: Reader String String
hello = asks $ \name -> "hello, " ++ name ++ "!"

bye :: Reader String String
bye = asks $ \name -> "bye, " ++ name ++ "!"

convo :: Reader String String
convo = asks (const (++)) <*> hello <*> bye

main :: IO ()
main = print . runReader convo $ "adit"