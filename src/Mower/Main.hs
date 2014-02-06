module Main where

import System.IO

import Data.Maybe
import Control.Monad.State

import Mower.Core
import Mower.Factory
import Mower.Parser
import Mower.Types

-- Lazy eval
withLineNumber :: [String] -> [(Int, String)]
withLineNumber = zip [1..]

main :: IO ()
main = do
	handle <- openFile "resources/commands.txt" ReadMode
	content <- hGetContents handle
	let emptyField = fromJust (makeEmptyField 0 0)
	let board = execState (makeBoard $ withLineNumber $ (lines content)) (Board emptyField [])
	print $ execState (playGame (players board) ) (field board)
	hClose handle