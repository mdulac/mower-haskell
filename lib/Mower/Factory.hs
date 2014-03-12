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
makePosition = undefined

toCommand :: Char -> Maybe Command
toCommand = undefined

makeCommands :: String -> Maybe [Command]
makeCommands = undefined

makeEmptyField :: Int -> Int -> Maybe Field
makeEmptyField = undefined

makeMower :: Int -> Int -> Direction -> Maybe Mower
makeMower = undefined

makePlayer :: Mower -> [Command] -> Maybe Player
makePlayer = undefined

toDirection :: Char -> Maybe Direction
toDirection = undefined