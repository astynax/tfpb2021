{-# OPTIONS -Wall -Wno-unused-top-binds #-}

module Lesson03 where

-- Peano numbers

data Nat = Zero | Succ Nat

-- zero = Zero
-- three = Succ (Succ (Succ Zero))

atNat :: Nat -> [a] -> Maybe a
atNat Zero     (x:_  ) = Just x
atNat (Succ i) (_: xs) = atNat i xs
atNat _        []      = Nothing

-- matrix 4x4

data X
data Y

data Vec4 tag a = Vec4 a a a a deriving Show
type Vec2d a = Vec4 Y (Vec4 X a)
type Pos = (D4 X, D4 Y)

data D4 tag = D1 | D2 | D3 | D4 deriving (Show, Eq)

addD4 :: D4 tag -> D4 tag -> Maybe (D4 tag)
addD4 x y = fromInt (toInt x + toInt y)

toInt :: D4 t -> Int
toInt D1 = 1
toInt D2 = 2
toInt D3 = 3
toInt D4 = 4

fromInt :: Int -> Maybe (D4 t)
fromInt x = case x of
  1 -> Just D1
  2 -> Just D2
  3 -> Just D3
  4 -> Just D4
  _ -> Nothing

addD4' :: D4 tag -> D4 tag -> Maybe (D4 tag)
addD4' x y =
  case (decD4 x, incD4 y) of
    (Just x', Just y') -> addD4' x' y'
    (_,       z)       -> z

decD4 :: D4 t -> Maybe (D4 t)
decD4 D2 = Just D1
decD4 D3 = Just D2
decD4 D4 = Just D3
decD4 _  = Nothing

incD4 :: D4 t -> Maybe (D4 t)
incD4 D1 = Just D2
incD4 D2 = Just D3
incD4 D3 = Just D4
incD4 _  = Nothing

at :: D4 tag -> Vec4 tag a -> a
at D1 (Vec4 x _ _ _) = x
at D2 (Vec4 _ x _ _) = x
at D3 (Vec4 _ _ x _) = x
at D4 (Vec4 _ _ _ x) = x

mapVec4
  :: (a -> a)
  -> D4 tag
  -> Vec4 tag a
  -> Vec4 tag a
mapVec4 f D1 (Vec4 x b c d) = Vec4 (f x) b c d
mapVec4 f D2 (Vec4 a x c d) = Vec4 a (f x) c d
mapVec4 f D3 (Vec4 a b x d) = Vec4 a b (f x) d
mapVec4 f D4 (Vec4 a b c x) = Vec4 a b c (f x)

-- (.) :: (a -> b) -> (c -> a) -> c -> b
-- ($) :: (a -> b) -> a -> b

at2d :: D4 X -> D4 Y -> Vec2d a -> a
at2d x y = at x . at y

map2d :: (a -> a) -> D4 X -> D4 Y -> Vec2d a -> Vec2d a
map2d f x y = mapVec4 (mapVec4 f x) y

set2d :: a -> D4 X -> D4 Y -> Vec2d a -> Vec2d a
set2d = map2d . const

data Piece

data Color = Red | Blue deriving Eq

type Field = Vec2d (Maybe (Color, D4 Piece))

-- data Field' = Field'
--   { red1 :: Maybe Pos
--   , ...
--   , blue4 :: Maybe Pos
--   }

moves
  :: Color
  -> Field
  -> [(Pos, Pos)]
  -> Maybe (Color, Field)
moves c f []     = Just (c, f)
moves c f (m:ms) = error "TODO: implement me!"

move :: Pos -> Pos -> Field -> Maybe Field
move (x0, y0) (x1, y1) f
  | not (near x0 x1 || near y0 y1) = Nothing
  | otherwise =
  case (at2d x0 y0 f, at2d x1 y1 f) of
    (Nothing, _) -> Nothing

    (Just x, Nothing) -> jumpWith x

    (Just (c0, v0), Just (c1, v1))
      | c0 == c1 ->
        case addD4 v0 v1 of
          Just s -> jumpWith (c0, s)
          Nothing -> Nothing
      | otherwise ->
          jumpWith $ case fight v0 v1 of
            Left v0' -> (c0, v0')
            Right v1' ->(c1, v1')
  where
    near v0 v1 =
      decD4 v0 == Just v1
      || incD4 v0 == Just v1
    jumpWith v = Just
      $ set2d Nothing x0 y0
      $ set2d (Just v) x1 y1 f

fight :: D4 tag -> D4 tag -> Either (D4 tag) (D4 tag)
fight x y =
  case decD4 y of
    Nothing -> Left x
    Just y' -> case decD4 x of
      Nothing -> Right y'
      Just x' -> fight x' y'

fight' :: D4 tag -> D4 tag -> Either (D4 tag) (D4 tag)
fight' x y =
  case (decD4 x, decD4 y) of
    (_, Nothing) -> Left x
    (Nothing, Just y') -> Right y'
    (Just x', Just y') -> fight' x' y'

areEqual
  :: Eq a
  => (D4 t -> D4 t -> a)
  -> (D4 t -> D4 t -> a)
  -> Bool
areEqual f1 f2 = and
  [ f1 x y == f2 x y
  | x <- [D1, D2, D3, D4]
  , y <- [D1, D2, D3, D4]
  ]

bothFightsAreEqual :: Bool
bothFightsAreEqual = areEqual fight fight'
