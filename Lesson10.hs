{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lesson10 where

import Debug.Trace
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Control.Monad.Trans.Reader as T
import qualified Control.Monad.Trans.State as T

class Monad m => MonadReader r m | m -> r where
  ask :: m r

class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

class Monad m => MonadLogger m where
  debug :: String -> m ()

loggingStep ::
  ( MonadReader Int m
  , MonadState Int m
  , MonadLogger m
  ) => m ()
loggingStep = do
  current <- step
  debug $ "Current: " <> show current

step ::
  ( MonadReader Int m
  , MonadState Int m
  ) => m Int
step = do
  x <- ask
  s <- get
  let current = s + x
  put current
  pure current

type AppM = T.ReaderT Int (T.StateT Int IO)
type AppM' = T.StateT Int (T.ReaderT Int IO)

runAppM :: Int -> AppM a -> IO a
runAppM step action = T.evalStateT (T.runReaderT action step) 0

runAppM' :: Int -> AppM' a -> IO a
runAppM' step action = T.runReaderT (T.evalStateT action 0) step

instance Monad m => MonadReader r (T.ReaderT r m) where
  ask = T.ask

instance MonadState s m => MonadState s (T.ReaderT r m) where
  get = lift get
  put = lift . put

instance Monad m => MonadState s (T.StateT s m) where
  get = T.get
  put = T.put

instance MonadReader r m => MonadReader r (T.StateT s m) where
  ask = lift ask

instance (Monad m, MonadIO m) => MonadLogger m where
  debug = liftIO . putStrLn

-- instance MonadLogger m => MonadLogger (T.ReaderT r m) where
--   debug = lift . debug

-- instance MonadLogger m => MonadLogger (T.StateT s m) where
--   debug = lift . debug

newtype OurM a
  = OurM { runOurM :: Int -> Int -> IO (a, Int) }
  deriving Functor

instance Applicative OurM where
  pure x = OurM $ \_ s -> pure (x, s)
  af <*> ax = OurM $ \r s -> do
    (f, s') <- runOurM af r s
    (x, s'') <- runOurM ax r s'
    pure (f x, s'')

instance Monad OurM where
  mx >>= f = OurM $ \r s -> do
    (x, s') <- runOurM mx r s
    runOurM (f x) r s'

instance MonadReader Int OurM where
  ask = OurM $ \r s -> pure (r, s)

instance MonadState Int OurM where
  get = OurM $ \_ s -> pure (s, s)
  put x = OurM $ \_ _ -> pure ((), x)

instance MonadIO OurM where
  liftIO x = OurM $ \_ s -> do
    r <- x
    pure (r, s)

-- debugging

fac 0 = 1
fac n = n * fac (n - 1)

traceFn :: (Show a, Show b) => String -> (a -> b) -> (a -> b)
traceFn name f arg = trace (name <> "(" <> show arg <> ") => " <> show res) res
  where
    res = f arg
