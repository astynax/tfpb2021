{-# OPTIONS -Wall #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeOperators #-}

module Lesson02 where

import GHC.Types (Type)

type Pair :: Type -> Type
data Pair a = MkPair a a

type Unit :: Type
data Unit = Unit

type Void' :: Type
data Void'

f :: Void'
f = f

data Person extra = Person
  { pName :: Name
  , pAge :: Int
  , pExtra :: extra
  }

data Name
  = JustName String
  | Id Int
  | SSN Int
  | Noname

  -- Either String (Either Int (Either Int ()))

data Color key = Red | Black | OfVoid key

colorOfVoid :: Color Void'
colorOfVoid = OfVoid f

isVoidColored :: Color Void' -> Bool
isVoidColored (OfVoid _) = True
isVoidColored _          = False

data Pet = Dog String | Snail | Ants Int

bobWithAnts :: Person Pet
bobWithAnts = Person
  { pName = SSN 42
  , pAge = 100
  , pExtra = Ants 13
  }

-- Person{pName = bobName} = bobWithAnts
-- x = case bobName of
--   JustName v -> v
--   _ -> "???"

data User a
    = Registered (Person a)
    | Guest

hasManyAnts :: User (Maybe Pet) -> Bool
hasManyAnts u = case u of
  Registered (Person {pExtra = Just (Ants n)})
    | n >= 100 -> True
  _ -> False

itIsFalse :: Bool
itIsFalse =
  let jake = Person (JustName "Jake") 16 Nothing
  in hasManyAnts (Registered jake)

-- Kanban
data Desk = Desk { boards :: [(BoardName, [Card])] }

data Card =
  Card
  { key :: CardId
  , title :: String
  , body :: String
  , color :: Color Void'
  , author :: User ()
  , assignees :: [User ()]
  }

data Err
  = BadCardId CardId
  | BadBoardName BoardName

newtype CardId = CardId Integer
newtype BoardName = BoardName String

move :: CardId -> BoardName -> Desk -> Either Err Desk
move = undefined

-- Game
data X
data Y

newtype Coord axis = Coord Int

x0 :: Coord X
x0 = Coord 0

newtype MUnit system name = MUnit Float

data Imperial
data Metric

data Distance
data Time

data In a b

data a :/ b
infixr 5 :/

type Speed system = MUnit system (Distance `In` Time)
type Seconds = MUnit Metric Time
type Meters = MUnit Metric Distance

speed :: Speed Metric
speed = MUnit 250

shift
  :: Speed sys
  -> MUnit sys Time
  -> MUnit sys Distance
shift = mult

mult
  :: MUnit s (a `In` b)
  -> MUnit s b
  -> MUnit s a
mult (MUnit s) (MUnit t) = MUnit (s * t)
