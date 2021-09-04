{-# OPTIONS -Wall -Wno-type-defaults #-}

module Lesson01 where

name :: [Char]
name = "asdda"

ch :: Char
ch = 'a'

mulBy10 :: Int -> Int
mulBy10 x = x * 10

doubleSum :: Int -> Int -> Int
doubleSum x y = res
  where
    res = s + s
    s = x + y

sign :: (Ord a, Num a) => a -> Int
sign x
  | x < 0 = -1
  | x > 0 = 1
sign _    = 0

bothZeroes :: Int -> Int -> Bool
bothZeroes 0 0 = True
bothZeroes _ _ = False

isA :: Char -> Bool
isA 'a' = True
isA 'A' = True
isA _   = False

sign' :: (Num a, Ord a) => a -> Int
sign' x =
  case x of
    _
     | x < 0  -> -1
     | x > 0  -> 1
    _         -> 0

isA' :: Char -> Bool
isA' x
  | isLetterA = True
  | otherwise = False
  where
    isLetterA = x == 'a' || x == 'A'

(+++) :: Num a => a -> a -> a
x +++ y = x + y + 1

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

fun :: a -> b -> c -> d
fun = undefined

fun' :: b -> d
fun' = fun 1 `flip` 3

id' :: a -> a
id' x = x

undefined' :: a
undefined' = error "oops"

const' :: a -> b -> a
const' x _ = x

somethingWithInt :: Integer -> Integer
somethingWithInt _ = 7

somethingWithBool :: Bool -> Bool
somethingWithBool _ = False

some :: [a] -> a
some l = head l

safeHead :: a -> [a] -> a
safeHead d []    = d
safeHead _ (x:_) = x

true :: Bool
true =
     [1, 2, 3] == (1 : [2, 3])
  && [1, 2, 3] == 1 : 2 : [3]
  && [1, 2, 3] == 1 : 2 : 3 : []

len :: [a] -> Int
len []       = 0
len (_ : xs) = 1 + len xs

len' :: [a] -> Int
len' = go 0
  where
    go acc []     = acc
    go acc (_:xs) = go (acc + 1) xs

summ :: [Int] -> Int
summ [] = 0
summ (x:xs) = x + summ xs
