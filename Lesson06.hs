{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}

module Lesson06 where

calc :: Float -> Either String Float
calc = undefined

res :: Either String Float
res = calc 1

-- (+ 1) :: Float -> Float

mapResult
  :: (a -> b)
  -> Either String a -> Either String b
mapResult = undefined

{-
class Functor (f :: Type -> Type) where
  fmap :: (a -> b) -> f a -> f b
  -- 1) fmap id x == x
  -- 2) fmap f . fmap g == fmap (f . g)

  f <$> fx
  f  $   x
-}

newtype Identity a =
  Identity { runIdentity :: a } deriving (Show)

instance Functor Identity where
  fmap f = Identity . f . runIdentity

-- instance Functor Maybe where
--   fmap _ Nothing = Nothing
--   fmap f (Just x) = Just (f x)

data BTree a = BNode a (BTree a) (BTree a) | BLeaf

instance Functor BTree where
   fmap _ BLeaf = BLeaf
   fmap f (BNode x t1 t2) =
     BNode (f x) (fmap f t1) (fmap f t2)

-- class Functor f => Applicative f where
--   pure  :: a -> f a
--   (<*>) :: f (a -> b) -> f a -> f b
--   TODO: laws

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = pure (f x)

-- instance Applicative Maybe where
--   pure = Just
--   Just f <*> Just x = Just (f x)
--   _      <*> _      = Nothing
--   (<* ) :: f a -> f b -> f a
--   ( *>) :: f a -> f b -> f b

--   (a -> b) -> f a -> f b
-- f (a -> b) -> f a -> f b
-- (a ->   b) . (b ->   c)
-- (a -> f b) ? (b -> f c)

-- (a -> Maybe b) <$> Maybe a ==> Maybe (Maybe b)

-- class Applicative f => Monad where
--   join :: f (f a) -> f a
--   (>>=) :: f a -> (a -> f b) -> f b
--   --   pure a >>= (a -> f b) >>= (b -> f c) >>= ...
--   (>>) :: f a -> f b -> f b
--   x >> y = x >>= \_ -> y

inputUser :: IO (String, Int)
inputUser =
  putStrLn "Enter name and age" >>= (\_ ->
  getLine                       >>= (\name ->
  getLine                       >>= (\strAge ->
  pure (name, read strAge))))

inputUser' :: IO (String, Int)
inputUser' = do
  putStrLn "Enter name and age"
  name <- getLine
  age  <- readLn
  pure (name, age)

data User = User
  { name    :: String
  , surname :: String
  , age     :: Int
  } deriving (Show)

readUser :: IO User
readUser =
  User <$> (putStrLn "Enter name:"    *> getLine)
       <*> (putStrLn "Enter surname:" *> getLine)
       <*> (putStrLn "Enter age:"     *> readLn)

readUser' :: IO User
readUser' = do
  putStrLn "Enter name: "
  name <- getLine
  putStrLn "Enter surname: "
  surname <- getLine
  putStrLn "Enter age: "
  age <- readLn
  pure User{..}

fullName User{..} = name <> " " <> surname

-- (<$>)    (a ->   b) -> f a -> f b
-- (<*>)  f (a ->   b) -> f a -> f b
-- (=<<)    (a -> f b) -> f a -> f b

maybeSum :: Maybe Int -> Maybe Int -> Maybe Int
maybeSum mx my = do
  x <- mx
  y <- my
  pure (x + y)

-- TASK:

data List a = Nil | Cons !a (List a) deriving (Show)
newtype ZipList a
  = ZipList { getZipList :: [a] }
  deriving (Show)

-- Need to implement Functor, Applicative, Monad

instance Semigroup (List a) where
  Nil       <> l  = l
  Cons x xs <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure :: a -> List a
  pure x = Cons x Nil

  (<*>) :: List (a -> b) -> List a -> List b
  Nil       <*> _  = Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  (>>=) :: List a -> (a -> List b) -> List b
  Nil       >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

-- ZipList [f1, f2] <*> ZipList [x1, x2]
--   == ZipList [f1 x1, f2 x2]

instance Functor ZipList where
  fmap f (ZipList xs) = ZipList (fmap f xs)

instance Applicative ZipList where
  pure = ZipList . pure
  -- ZipList [] <*> _         = ZipList []
  -- ZipList _ <*> ZipList [] = ZipList []
  -- ZipList (f:fs) <*> ZipList (x:xs) =
  --   ZipList (f x : getZipList (ZipList fs <*> ZipList xs))
  fs <*> xs =
    ZipList (zipWith ($)
             (getZipList fs)
             (getZipList xs))
