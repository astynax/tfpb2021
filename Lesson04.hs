{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS -Wall -Wno-missing-signatures #-}

-- | "Typeclasses"

module Lesson04 where

import Data.Void
import Data.Proxy

{-

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}

-}

data D4 = D1 | D2 | D3 | D4 deriving Show

data V4 a = V4 a a a a

instance Eq D4 where
  D1 == D1 = True
  D2 == D2 = True
  D3 == D3 = True
  D4 == D4 = True
  _  == _  = False

{-

class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<) :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>) :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max :: a -> a -> a
  min :: a -> a -> a
  {-# MINIMAL compare | (<=) #-}

data Ordering = LT | EQ | GT

-}

instance Ord D4 where
  compare a b = case (a, b) of
    _ | a == b -> EQ
    (_, D4)    -> GT
    (D4, _)    -> LT
    (_, D3)    -> GT
    (D3, _)    -> LT
    (_, D2)    -> GT
    (D2, _)    -> LT
    _          -> error "impossible"

areEqual = D4 >= D4

instance Eq a => Eq (V4 a) where
  V4 a1 b1 c1 d1 == V4 a2 b2 c2 d2 =
    (a1 == a2)
    && (b1 == b2)
    && (c1 == c2)
    && (d1 == d2)

class HasSize a where
  sizeOf :: Int

instance HasSize D4 where
  sizeOf = 4

instance (HasSize a, HasSize b) => HasSize (a, b) where
  sizeOf = sizeOf @a * sizeOf @b

instance HasSize Bool where
  sizeOf = 2

instance HasSize () where
  sizeOf = 1

instance HasSize Void where
  sizeOf = 0

instance HasSize a => HasSize (V4 a) where
  sizeOf = 4 * sizeOf @a

data Nil = Nil
data Cons a b = Cons a b

type V3 a = Cons a (Cons a (Cons a Nil))
type V3' a = (a, (a, (a, ())))

instance HasSize Nil where
  sizeOf = 1

instance
  (HasSize a, HasSize b) =>
  HasSize (Cons a b) where
    sizeOf = sizeOf @a * sizeOf @b

-- slozhno!

data Z
data S a

type Third = S (S (S Z))

-- data Proxy a = Proxy

fromInt :: forall ix. Int -> Maybe (Proxy ix)
fromInt = undefined

-- -- at @Third V3 => a

cons :: a -> b -> Cons a b
cons x y = Cons x y

single :: a -> Cons a Nil
single a = Cons a Nil

-- class HasElem ix (t a) where
--   at :: t a -> a
--
-- instance HasElem Z (Cons a b) where
--   at (Cons x _) = x
--
-- instance HasElem (S i) (Cons _ a) where
--   at (Cons _ x) = at @i x

{-

class Show a where
  show :: a -> String

-}

newtype Password = Password String

instance Show Password where
  show _ = "***"

-- Expression problem

-- data Shape = Circle | Square | Line | Rhombus
--
-- paint :: Shape -> Picture
-- outline :: Shape -> Picture

-- class FromJSON a where
--   decode :: String -> Maybe a

-- class ToJSON a where
--   encode :: a -> String

-- instance FromJSON RBTree where

-- type Errors = '["ErrorFoo", "ErrorBar"]

-- f :: HasErrors (Cons ErrFoo (Cons ErrBar m)) => m a
-- f = undefined

-- catch :: HasError ErrFoo m a => a -> m a -> m a
