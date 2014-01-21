{-# LANGUAGE TypeFamilies #-}

module Mower.Core (
      Position(..)
    , Command(..)
    , Direction(..)
    , Mower(..)
    , Field(..)
    , Player(..)
    , Board(..)
    , turnLeft
    , turnRight
    , forward
    , toDirection
    , toCommand
    , computeCommand
    , computeCommands
    , playGame
    , makeConfig
    , parseField
    , parsePlayer
    , makeEmptyField
    , makeMower
    , makePlayer
    , makeCommands
    ) where

import System.IO()
import Data.Monoid
import Data.List()
import Data.Either()
import Control.Monad.State

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

newtype Position = Position (Int, Int) deriving Eq

data Command = L | R | F deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Mower = Mower { position :: Position, direction :: Direction } deriving (Eq)
data Field = Field { corner :: Position, mowers :: [Mower] } deriving Show
data Player = Player { mo :: Mower, co :: [Maybe Command] } deriving Show
data Board = Board { f :: Field, p :: [Player] } deriving Show

instance Show Mower where
	show (Mower p d) = "Mower @ " ++ show p ++ " facing " ++ show d

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

turnLeft :: Mower -> Mower
turnLeft (Mower p North) = Mower p West
turnLeft m = Mower (position m) $ pred (direction m)

turnRight :: Mower -> Mower
turnRight (Mower p West) = Mower p North
turnRight m = Mower (position m) $ succ (direction m)

forward :: Mower -> Mower
forward m@(Mower p North) = Mower p' North where p' = mappend p (Position (1, 0))
forward m@(Mower p East) = Mower p' East where p' = mappend p (Position (0, 1))
forward m@(Mower p South) = Mower p' South where p' = mappend p (Position (-1, 0))
forward m@(Mower p West) = Mower p' West where p' = mappend p (Position (0, -1))

-- Monoid handling
forwardToValidPosition :: Field -> Mower -> Mower
forwardToValidPosition f m = do
	let m' = forward m
	let p' = position m'
	if (isValidPosition p' f) then m' else m

-- Guards
isValidPosition :: Position -> Field -> Bool
isValidPosition p@(Position (x, y)) f@(Field _ mowers)
	| x < 0 = False
	| y < 0 = False
	| p > p' = False
	| elem p (map ( \m -> position m ) mowers ) = False
	| otherwise = True
	where p' = corner f

toDirection :: Char -> Direction
toDirection 'N' = North
toDirection 'W' = West
toDirection 'S' = South
toDirection 'E' = East

toCommand :: Char -> Maybe Command
toCommand 'G' = Just L
toCommand 'D' = Just R
toCommand 'A' = Just F
toCommand _ = Nothing

computeCommand :: Command -> Field -> Mower -> Mower
computeCommand c f
	| c == L = turnLeft
	| c == R = turnRight
	| c == F = forwardToValidPosition f
	| otherwise = id

-- State Monad
computeCommands :: Maybe [Command] -> Field -> State Mower ()
computeCommands Nothing f = state ( \m -> ((), m) )
computeCommands ( Just [] ) f = state ( \m -> ((), m) )
computeCommands ( Just (c:xc) ) f = state ( \m -> ((), computeCommand c f m) ) >>= ( \m -> computeCommands (Just xc) f )

-- State Monad
playGame :: [Player] -> State Field ()
playGame [] = state ( \f -> ((), f) )
playGame (p:xp) = state ( \f -> do
	let m = execState (computeCommands (sequence $ co p) f ) (mo p)
	if (isValidPosition (position $ mo p) f) then ( (), Field (corner f) ( m : (mowers f)) ) else ( (), f )
	) >>= ( \f -> playGame xp )

makeConfig :: [(Int, String)] -> State Board ()
makeConfig [] = state ( \c -> ((), c))
makeConfig ((1, l):ls) = state ( \c -> ((), Board (parseField l) []) ) >>= ( \c -> makeConfig ls )
makeConfig ((_, l):ls) = state ( \c -> ((), Board (f c) (parsePlayer l : (p c))) ) >>= ( \c -> makeConfig ls )

parseField :: String -> Field
parseField line = do
	let f = parse fieldParser "" line
	case f of
		Right f -> f

parsePlayer :: String -> Player
parsePlayer line = do
	let p = parse playerParser "" line
	case p of
		Right p -> p

-- Parsers

playerParser :: Parser Player
playerParser = do
	x <- many1 digit
	_ <- space
	y <- many1 digit
	_ <- space
	d <- oneOf "NESW"
	_ <- space
	cs <- many $ oneOf "GAD"
	return $ makePlayer ( makeMower (read x :: Int) (read y :: Int) (toDirection d) ) ( makeCommands cs )

fieldParser :: Parser Field
fieldParser = do
	x <- many1 digit
	_ <- space
	y <- many1 digit
	return $ makeEmptyField (read x :: Int) (read y :: Int)

-- Factories

makeEmptyField :: Int -> Int -> Field
makeEmptyField x y = Field (Position (x, y)) []

makeMower :: Int -> Int -> Direction -> Mower
makeMower x y d = Mower (Position (x, y)) d

makePlayer :: Mower -> [Maybe Command] -> Player
makePlayer = Player

makeCommands :: String -> [Maybe Command]
makeCommands = map toCommand
