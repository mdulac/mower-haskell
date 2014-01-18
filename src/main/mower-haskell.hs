import System.IO
import Data.Monoid
import Data.List
import Data.Either
import Control.Monad.State

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)

newtype Position = Position (Int, Int) deriving Eq

data Command = L | R | F deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq, Enum)
data Mower = Mower { position :: Position, direction :: Direction } deriving (Eq)
data Field = Field { corner :: Position, mowers :: [Mower] } deriving Show

data Player = Player { mo :: Mower, co :: [Maybe Command] } deriving Show
data Configuration = Configuration { f :: Field, p :: [Player] } deriving Show

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

-- Monoid handling
forward :: Field -> Mower -> Mower
forward f m@(Mower p North) = if (isValidPosition p' f) then Mower p' North else m where p' = mappend p (Position (1, 0))
forward f m@(Mower p East) = if (isValidPosition p' f) then Mower p' East else m where p' = mappend p (Position (0, 1))
forward f m@(Mower p South) = if (isValidPosition p' f) then Mower p' South else m where p' = mappend p (Position (-1, 0))
forward f m@(Mower p West) = if (isValidPosition p' f) then Mower p' West else m where p' = mappend p (Position (0, -1))

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
	| c == F = forward f
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

-- Lazy eval
withLineNumber :: [String] -> [(Int, String)]
withLineNumber = zip [1..]

makeConfig :: [(Int, String)] -> State Configuration ()
makeConfig [] = state ( \c -> ((), c))
makeConfig ((1, l):ls) = state ( \c -> ((), Configuration (parseField l) []) ) >>= ( \c -> makeConfig ls )
makeConfig ((_, l):ls) = state ( \c -> ((), Configuration (f c) (parsePlayer l : (p c))) ) >>= ( \c -> makeConfig ls )

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
	space
	y <- many1 digit
	space
	d <- oneOf "NESW"
	space
	cs <- many $ oneOf "GAD"
	return $ makePlayer ( makeMower (read x :: Int) (read y :: Int) (toDirection d) ) ( makeCommands cs )

fieldParser :: Parser Field
fieldParser = do
	x <- many1 digit
	space
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

main = do
	handle <- openFile "src/resources/commands.txt" ReadMode
	content <- hGetContents handle

	let field = makeEmptyField 0 0
	let conf = execState (makeConfig $ withLineNumber $ (lines content)) (Configuration field [])

	print $ execState (playGame (p conf) ) (f conf)

	hClose handle