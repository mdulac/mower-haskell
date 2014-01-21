module Main where

import System.IO
import Control.Monad.State
import Mower.Core

-- Lazy eval
withLineNumber :: [String] -> [(Int, String)]
withLineNumber = zip [1..]

main :: IO ()
main = do
	handle <- openFile "resources/commands.txt" ReadMode
	content <- hGetContents handle

	let field = makeEmptyField 0 0
	let board = execState (makeConfig $ withLineNumber $ (lines content)) (Board field [])

	print $ execState (playGame (p board) ) (f board)

	hClose handle