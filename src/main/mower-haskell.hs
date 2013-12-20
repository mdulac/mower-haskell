import System.IO
import Data.Monoid
import Data.List
import Control.Monad.State

newtype Position = Position (Int, Int) deriving Eq

data Command = L | R | F deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Mower = Mower { position :: Position, direction :: Direction } deriving (Eq)
data Field = Field { corner :: Position, mowers :: [Mower] } deriving Show
data Player = Player { mower :: Mower, commands :: [Command] } deriving Show

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
	Position (x, y) > Position (x', y') = x > x' && y > y'

turnLeft :: Mower -> Mower
turnLeft (Mower p West) = Mower p North
turnLeft m = Mower (position m) $ succ (direction m)

turnRight :: Mower -> Mower
turnRight (Mower p North) = Mower p West
turnRight m = Mower (position m) $ pred (direction m)

-- Monoid handling
forward :: Mower -> Mower
forward m@(Mower p North) = Mower (mappend p (Position (1, 0))) North
forward m@(Mower p East) = Mower (mappend p (Position (0, 1))) East
forward m@(Mower p South) = Mower (mappend p (Position (-1, 0))) South
forward m@(Mower p West) = Mower (mappend p (Position (0, -1))) West

-- Guards
isValidPosition :: Position -> Field -> Bool
isValidPosition p@(Position (x, y)) f
	| x < 0 = False
	| y < 0 = False
	| p > p' = False
	| otherwise = True
	where p' = corner f

toCommand :: Char -> Maybe Command
toCommand 'G' = Just L
toCommand 'D' = Just R
toCommand 'A' = Just F
toCommand _ = Nothing

computeCommand :: Command -> Mower -> Mower
computeCommand c
	| c == L = turnLeft
	| c == R = turnRight
	| c == F = forward
	| otherwise = id

-- State Monad
computeCommands :: [Command] -> State Mower ()
computeCommands [] = state ( \m -> ((), m) )
computeCommands (c:xc) = state ( \m -> ((), computeCommand c m) ) >>= ( \m -> computeCommands xc )

-- State Monad
playGame :: [Player] -> State Field ()
playGame [] = state ( \f -> ((), f) )
playGame (p:xp) = state (\f ->
	let mo = execState (computeCommands c) m in ((), Field (corner f) ((mowers f) ++ [mo])) ) >>= ( \f -> playGame xp )
	where
		c = commands p
		m = mower p

main = do
	handle <- openFile "src/resources/commands.txt" ReadMode
	content <- hGetContents handle
	let contents = lines $ content

	let p = mempty
	let d = North
	let m = Mower p d

	print $ execState (computeCommands [L, L, F, F]) m
	let result = execState (playGame [Player m [L, L, F, F], Player m [L, L, L], Player m [F, F, F]]) (Field p [])
	print $ "RESULT IS : " ++ show result

	hClose handle