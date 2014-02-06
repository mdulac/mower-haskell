module Mower.Parser (
	parseField,
	parsePlayer
	) where

import Text.Parsec hiding (State)
import Text.Parsec.String (Parser)
import Mower.Types
import Mower.Factory

playerParser :: Parser (Maybe Player)
playerParser = do
	x <- many1 digit
	_ <- space
	y <- many1 digit
	_ <- space
	d <- oneOf "NESW"
	_ <- space
	cs <- many $ oneOf "GAD"

	let z = do
		direction <- toDirection d
		m <- makeMower (read x :: Int) (read y :: Int) direction
		c <- makeCommands cs
		makePlayer m c

	return z
	
fieldParser :: Parser (Maybe Field)
fieldParser = do
	x <- many1 digit
	_ <- space
	y <- many1 digit
	return $ makeEmptyField (read x :: Int) (read y :: Int)

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