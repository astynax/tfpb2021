{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | Monad Transformers

module Lesson09 where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Trans.Maybe
-- import Lesson08

newtype ReaderT r m a =
  ReaderT { runReaderT :: r -> m a }

-- newtype Identity a = Identity { runIdentity :: a }

type Reader r a = ReaderT r Identity a

runReader :: Reader env a -> env -> a
runReader x env = runIdentity $ runReaderT x env

instance Functor m => Functor (ReaderT env m) where
  fmap f r = ReaderT $ \env ->
    fmap f (runReaderT r env)

instance Applicative m => Applicative (ReaderT env m) where
  pure x = ReaderT $ \_ -> pure x
  af <*> ax = ReaderT $ \env ->
    let
      f = runReaderT af env
      x = runReaderT ax env
    in f <*> x

instance Monad m => Monad (ReaderT env m) where
  (>>=) :: ReaderT env m a
        -> (a -> ReaderT env m b)
        -> ReaderT env m b
  mx >>= f = ReaderT $ \env -> do
    x <- runReaderT mx env
    runReaderT (f x) env

ask :: Applicative m => ReaderT env m env
ask = ReaderT $ \env -> pure env

local :: (env -> env) -> ReaderT env m a -> ReaderT env m a
local f x = ReaderT $ \env -> runReaderT x (f env)

foo :: ReaderT Int IO ()
foo = do
  putLn "{"
  local (+ 2) $ do
    putLn "a"
    local (+ 1) $ putLn "b"
    putLn "c"
  putLn "}"
  where
    putLn msg = do
      n <- ask
      lift $ putStrLn $ replicate n ' ' <> msg

-- class (forall m. Monad m => Monad (t m))
--   => MonadTrans t where
--   lift :: Monad m => m a -> t m a

instance MonadTrans (ReaderT env) where
  lift x = ReaderT $ \_ -> x

type CounterT m a = ReaderT Int (StateT Int m) a

runCounter :: Int -> IO Int
runCounter step = execStateT (runReaderT go step) 0
  where
    go = do
      inc
      trace
      inc
      trace
      inc
      trace
      inc
      trace
      inc
      trace
      dec
      trace
      dec
    trace = do
      v <- lift get
      lift $ lift $ putStrLn $ "Counter: " <> show v

inc, dec :: Monad m => CounterT m ()
inc = do
  step <- ask
  lift $ modify (+ step)

dec = do
  step <- ask
  lift $ modify (subtract step)

-- order of monads

type Stack1 m a = StateT Int (MaybeT m) a
type Stack2 m a = MaybeT (StateT Int m) a

-- parser

newtype ParserT m a =
  ParserT { runParserT :: StateT String (MaybeT m) a }
  deriving (Functor, Applicative, Monad, Alternative)

instance MonadTrans ParserT where
  lift = ParserT . lift . lift

type Parser a = ParserT Identity a

parse :: Monad m => ParserT m a -> String -> m (Maybe a)
parse p i = do
  res <- runMaybeT $ runStateT (runParserT p) i
  case res of
    Just (x, "") -> pure $ Just x
    _            -> pure Nothing

failure :: Monad m => ParserT m a
failure = ParserT $ lift $ fail "Oops"

anyChar :: Monad m => ParserT m Char
anyChar = do
  i <- ParserT get
  case i of
    (x:xs) -> do
      ParserT $ put xs
      pure x
    _ -> failure

char :: Monad m => Char -> ParserT m Char
char c = do
  x <- anyChar
  if c == x then pure c else failure

string :: Monad m => String -> ParserT m String
string ""     = pure ""
string (x:xs) = (:) <$> char x <*> string xs

touch :: ParserT IO ()
touch = do
  i <- ParserT get
  lift $ putStrLn $ "Input: " <> i

newtype AppT t a =
  AppT { runApp :: ReaderT Request (t IO) Response }
