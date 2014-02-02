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
    , forwardIfTargetPositionIsValid
    , toDirection
    , toCommand
    , computeCommand
    , computeCommands
    , playGame
    , makeBoard
    , parseField
    , parsePlayer
    , makeEmptyField
    , makeMower
    , makePlayer
    , makeCommands
    , makePosition
    ) where

import System.IO()
import Data.Monoid
import Data.List()
import Data.Either()
import Control.Monad.State

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

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

turnLeft :: Mower -> Mower
turnLeft (Mower pos North) = Mower pos West
turnLeft m = Mower (position m) $ pred (direction m)

turnRight :: Mower -> Mower
turnRight (Mower pos West) = Mower pos North
turnRight m = Mower (position m) $ succ (direction m)

forward :: Mower -> Mower
forward (Mower pos North) = Mower pos' North where pos' = mappend pos (Position (1, 0))
forward (Mower pos East) = Mower pos' East where pos' = mappend pos (Position (0, 1))
forward (Mower pos South) = Mower pos' South where pos' = mappend pos (Position (-1, 0))
forward (Mower pos West) = Mower pos' West where pos' = mappend pos (Position (0, -1))

-- Monoid handling
forwardIfTargetPositionIsValid :: Field -> Mower -> Mower
forwardIfTargetPositionIsValid f m = do
	let m' = forward m
	let p' = position m'
	if isValidPosition p' f then m' else m

-- Guards
isValidPosition :: Position -> Field -> Bool
isValidPosition pos@(Position (x, y)) f@(Field _ mowers)
	| x < 0 = False
	| y < 0 = False
	| pos > pos' = False
	| elem pos (map ( \m -> position m ) mowers ) = False
	| otherwise = True
	where pos' = corner f

toDirection :: Char -> Maybe Direction
toDirection 'N' = Just North
toDirection 'W' = Just West
toDirection 'S' = Just South
toDirection 'E' = Just East
toDirection _ = Nothing

toCommand :: Char -> Maybe Command
toCommand 'G' = Just L
toCommand 'D' = Just R
toCommand 'A' = Just F
toCommand _ = Nothing

computeCommand :: Command -> Field -> Mower -> Mower
computeCommand c f
	| c == L = turnLeft
	| c == R = turnRight
	| c == F = forwardIfTargetPositionIsValid f
	| otherwise = id

-- State Monad
computeCommands :: [Command] -> Field -> State Mower ()
computeCommands [] _ = state ( \m -> ((), m) )
computeCommands (c:xc) f = state ( \m -> ((), computeCommand c f m) ) >> ( computeCommands xc f )

-- State Monad
playGame :: [Player] -> State Field ()
playGame [] = state ( \f -> ((), f) )
playGame (p:xp) = state ( \f -> do
	let m = execState (computeCommands (commands p) f ) (mower p)
	if (isValidPosition (position $ mower p) f) then ( (), Field (corner f) ( m : (mowers f)) ) else ( (), f )
	) >> ( playGame xp )

makeBoard :: [(Int, String)] -> State Board ()
makeBoard [] = state ( \b -> ((), b))
makeBoard ((1, l):ls) = 
	case parseField l of 
		Nothing -> state ( \b -> ((), b) )
		Just f -> state ( \_ -> ((), Board f []) ) >> ( makeBoard ls )
makeBoard ((_, l):ls) =
	case parsePlayer l of
		Nothing -> state ( \b -> ((), b) ) >> ( makeBoard ls )
		Just p -> state ( \b -> ((), Board (field b) (p : (players b))) ) >> ( makeBoard ls )

parseField :: String -> Maybe Field
parseField line = do
	let f = parse fieldParser "" line
	case f of
		Left _ -> Nothing
		Right Nothing -> Nothing
		Right field -> field

parsePlayer :: String -> Maybe Player
parsePlayer line = do
	let p = parse playerParser "" line
	case p of
		Left _ -> Nothing
		Right Nothing -> Nothing
		Right player -> player

-- Parsers

playerParser :: Parser (Maybe Player)
playerParser = do
	x <- many1 digit
	_ <- space
	y <- many1 digit
	_ <- space
	d <- oneOf "NESW"
	_ <- space
	cs <- many $ oneOf "GAD"
	case toDirection d of
		Nothing -> return Nothing
		Just direction -> case makeMower (read x :: Int) (read y :: Int) direction of
			Nothing -> return Nothing
			Just m -> return $ makePlayer m ( makeCommands cs )
	
fieldParser :: Parser (Maybe Field)
fieldParser = do
	x <- many1 digit
	_ <- space
	y <- many1 digit
	return $ makeEmptyField (read x :: Int) (read y :: Int)

-- Factories

makeEmptyField :: Int -> Int -> Maybe Field
makeEmptyField x y = 
	case makePosition x y of
		Nothing -> Nothing
		Just p -> Just (Field p [])

makeMower :: Int -> Int -> Direction -> Maybe Mower
makeMower x y direction =
	case makePosition x y of
		Nothing -> Nothing
		Just position -> Just (Mower position direction)

makePlayer :: Mower -> [Maybe Command] -> Maybe Player
makePlayer mower commands =
	case sequence commands of
		Nothing -> Nothing
		Just cs -> Just (Player mower cs)

makeCommands :: String -> [Maybe Command]
makeCommands = map toCommand

makePosition :: Int -> Int -> Maybe Position
makePosition x y
	| x < 0 = Nothing
	| y < 0 = Nothing
	| otherwise = Just $ Position (x, y)