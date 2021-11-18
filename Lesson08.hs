{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
-- | Reader, Writer, State

module Lesson08 where

import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid (Sum(..))

newtype Reader env a = Reader { runReader :: env -> a }

ask :: Reader env env
ask = Reader id

asks :: (env -> a) -> Reader env a
asks = Reader

instance Functor (Reader env) where
  fmap f x = Reader $ \env -> f $ runReader x env

instance Applicative (Reader env) where
  pure = Reader . const
  af <*> ax = Reader $ \env ->
    let
      f = runReader af env
      x = runReader ax env
    in f x

instance Monad (Reader env) where
  (>>=) :: Reader env a
        -> (a -> Reader env b)
        -> Reader env b
  mx >>= f = Reader $ \env ->
    let x = runReader mx env
    in runReader (f x) env

data Config = Config
  { port   :: Int
  , host   :: String
  , silent :: Bool
  }

app :: Config -> IO ()
app = runReader $ do
  p <- asks port
  -- ...
  -- ...
  s <- asks silent
  if (not s)
    then do
      a <- addr
      pure $ putStrLn a
    else
      pure $ pure ()

  where
    addr = do
      p <- asks port
      h <- asks host
      pure $ h <> ":" <> show p

-- | Writer

newtype Writer w a = Writer { runWriter :: (a, w) }

tell :: w -> Writer w ()
tell x = Writer ((), x)

instance Functor (Writer w) where
  fmap f x =
    let (x', w) = runWriter x
    in Writer (f x', w)

instance Monoid w => Applicative (Writer w) where
  pure :: a -> Writer w a
  pure x = Writer (x, mempty)

  wf <*> wx =
    let
      (f, w1) = runWriter wf
      (x, w2) = runWriter wx
    in Writer (f x, w1 <> w2)

instance Monoid w => Monad (Writer w) where
  wx >>= f =
    let (x, w1) = runWriter wx
        (r, w2) = runWriter (f x)
    in Writer (r, w1 <> w2)

fizzBuzz n = snd $ runWriter (go 1)
  where
    go i = do
      if (i `mod` 15 == 0) then tell ["fizzbuzz"] else
        if (i `mod` 5 == 0) then tell ["buzz"] else
        if (i `mod` 3 == 0) then tell ["fizz"] else
          tell [show i]
      when (i < n) $
        go $ i + 1

fib = runWriter . go
  where
    go n = do
      tell (Sum 1, [n])
      case n of
        1 -> pure 1
        2 -> pure 1
        _ -> do
          a <- go (n - 1)
          b <- go (n - 2)
          pure (a + b)

-- | State

newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s -> a
evalState x = fst . runState x

execState :: State s a -> s -> s
execState x = snd . runState x

get :: State s s
get = State $ \s -> (s, s)

gets :: (s -> a) -> State s a
gets f = f <$> get

put :: s -> State s ()
put x = State $ \_ -> ((), x)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> ((), f s)
-- modify = get >>= put . f

instance Functor (State s) where
  fmap f sx = State $ \s ->
    let (x, s') = runState sx s
    in (f x, s')

instance Applicative (State s) where
  pure x = State $ \s -> (x, s)

  sf <*> sx = State $ \s ->
    let
      (f, s') = runState sf s
      (x, s'') = runState sx s'
    in (f x, s'')

instance Monad (State s) where
  sx >>= f = State $ \s ->
    let (x, s') = runState sx s
    in runState (f x) s'

fibS n = evalState (go n) Map.empty
  where
    go 1 = pure 1
    go 2 = pure 1
    go i = do
      cached <- gets (Map.lookup i)
      case cached of
        Just v -> pure v
        _      -> do
          a <- go (i - 1)
          b <- go (i - 2)
          let res = a + b
          modify $ Map.insert i res
          pure res

-- Pretty Printer

local f r = Reader $ runReader r . f

putLn x = ask >>= \n -> pure $ putStrLn $ replicate n ' ' <> x

instance Semigroup a => Semigroup (Reader r (IO a)) where
  r1 <> r2 = (<>) <$> r1 <*> r2

instance Monoid a => Monoid (Reader r (IO a)) where
  mempty = pure mempty

foo = mconcat
  [ putLn "{"
  , local (+ 2) $ mconcat
    [ putLn "a"
    , local (+ 1) $ putLn "b"
    , putLn "c"
    ]
  , putLn "}"
  ]
