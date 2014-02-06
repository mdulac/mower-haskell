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
-- TODO

toCommand :: Char -> Maybe Command
-- TODO

makeCommands :: String -> Maybe [Command]
-- TODO

makeEmptyField :: Int -> Int -> Maybe Field
-- TODO

makeMower :: Int -> Int -> Direction -> Maybe Mower
-- TODO

makePlayer :: Mower -> [Command] -> Maybe Player
-- TODO

toDirection :: Char -> Maybe Direction
-- TODO