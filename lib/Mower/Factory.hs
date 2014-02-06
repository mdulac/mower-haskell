module Mower.Factory (
	makePosition,
	toCommand,
	makeCommands,
	makeEmptyField,
	makeMower,
	makePlayer,
	toDirection
	) where

import Mower.Types

makePosition :: Int -> Int -> Maybe Position
makePosition x y
	| x < 0 = Nothing
	| y < 0 = Nothing
	| otherwise = Just $ Position (x, y)

toCommand :: Char -> Maybe Command
toCommand 'G' = Just L
toCommand 'D' = Just R
toCommand 'A' = Just F
toCommand _ = Nothing

makeCommands :: String -> Maybe [Command]
makeCommands = sequence . (map toCommand)

makeEmptyField :: Int -> Int -> Maybe Field
makeEmptyField x y = do
	p <- makePosition x y
	return $ Field p []

makeMower :: Int -> Int -> Direction -> Maybe Mower
makeMower x y direction = do
	p <- makePosition x y
	return $ Mower p direction

makePlayer :: Mower -> [Command] -> Maybe Player
makePlayer mower commands = return (Player mower commands)

toDirection :: Char -> Maybe Direction
toDirection 'N' = Just North
toDirection 'W' = Just West
toDirection 'S' = Just South
toDirection 'E' = Just East
toDirection _ = Nothing