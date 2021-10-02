{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Semigroup, Monoid, Foldable

module Lesson05 where

import Data.Monoid
import Data.Foldable

-- class Semigroup a where
--   (<>) :: a -> a -> a
--
--   a <> (b <> c) == (a <> b) <> c

-- instance Semigroup a => Semigroup (Maybe a)

-- import Data.Semigroup (First(..))
-- newtype First a = First { getFirst :: a }
--
-- first = First (First "a", "b") <> First (First "b", "c")
-- => First {getFirst = (First {getFirst = "a"},"b")}

-- class Semigroup a => Monoid a where
--   mempty :: a
--   mappend :: a -> a -> a
--   mappend = (<>)
--   mconcat :: [a] -> a
  -- mempty <> a  = a
  -- a <> memepty = a

-- newtype First a = First { getFirst :: Maybe a }

mbs :: [Maybe Int]
mbs = [Nothing, Just 42, Nothing, Nothing, Just 100]

check test =
  mconcat . map
  (\(i, x) -> let
      r = test x
      in
       ( Any r
       , All r
       , First $ if r then Just i else Nothing
       ))
   . zip [0..]

-- class Foldable (t :: Type -> Type) where
--   foldMap :: Monoid m => (a -> m) -> t a -> m

data BinTree a = BLeaf | BNode a (BinTree a) (BinTree a)
  deriving (Foldable, Functor)

data RoseTree a = Leaf a | Node [RoseTree a]

instance Foldable RoseTree where
  foldMap :: Monoid m => (a -> m) -> RoseTree a -> m
  foldMap f (Leaf a)  = f a
  foldMap f (Node ts) = foldMap (foldMap f) ts
                     -- mconcat (map (foldMap f) ts)

t :: RoseTree Int
t = Node
  [ Leaf 1
  , Node [ Leaf 100, Node [Leaf 50, Leaf 21], Leaf 3]
  , Leaf 700
  , Node [ Node [] ]
  , Leaf 42
  ]

t2 = BNode 7
     (BNode 5 (BNode 1 BLeaf BLeaf) BLeaf)
     (BNode 100 (BNode 75 BLeaf BLeaf)
      (BNode 101 BLeaf BLeaf))

newtype Flip a b = Flip { unFlip :: (b, a) }

data Cfg = Cfg
  { host :: Maybe String
  , port :: Maybe Int
  } deriving Show

instance Semigroup Cfg where
  (<>) = undefined  -- TODO

instance Monoid Cfg where
  mempty = Cfg { host = Just "localhost"
               , port = Just 8000
               }

main = print $ mconcat [mbFileCfg, mbEnv, mbCli]
  where
    mbFileCfg, mbEnv, mbCli :: Maybe Cfg
    mbFileCfg = Nothing
    mbEnv = Nothing
    mbCli = Nothing

-- class Functor (f :: Type -> Type) where
--   fmap :: (a -> b) -> f a -> f b
--
-- (<$>) = fmap
--
--   1) fmap id = id
--   2) fmap f . fmap g = fmap (f . g)

instance Functor RoseTree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node ts) = Node (fmap (fmap f) ts)

data V4 a = V4 a a a a deriving (Functor, Foldable)

search :: Ord a => a -> BinTree a -> Bool
search = undefined

singleton :: a -> BinTree a
singleton a = BNode a BLeaf BLeaf

insert :: Ord a => a -> BinTree a -> BinTree a
insert = undefined
