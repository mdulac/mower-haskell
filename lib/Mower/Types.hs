module Mower.Types (
	  Position(..)
    , Command(..)
    , Direction(..)
    , Mower(..)
    , Field(..)
    , Player(..)
    , Board(..)
	) where

import Data.List()
import Data.Monoid

newtype Position = Position (Int, Int) deriving Eq

data Command = L | R | F deriving (Show, Eq, Enum)
data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Mower = Mower { position :: Position, direction :: Direction } deriving (Eq)
data Field = Field { corner :: Position, mowers :: [Mower] } deriving (Show, Eq)
data Player = Player { mower :: Mower, commands :: [Command] } deriving (Show, Eq)
data Board = Board { field :: Field, players :: [Player] } deriving (Show, Eq)

instance Show Mower where
	show (Mower pos dir) = "Mower @ " ++ show pos ++ " facing " ++ show dir

instance Show Position where
	show (Position (x, y)) = "(" ++ show x ++ ", " ++ show y ++ ")"

-- Typeclass Monoid
instance Monoid Position where
	mempty = Position (0, 0)
	mappend (Position (x, y)) (Position (x', y')) = Position (x + x', y + y')

-- Typeclass Eq
instance Ord Position where
	Position (x, y) < Position (x', y') = x < x' && y < y'
	Position (x, y) > Position (x', y') = x > x' || y > y'