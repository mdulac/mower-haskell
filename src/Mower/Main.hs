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

	let f = makeEmptyField 0 0
	let b = execState (makeConfig $ withLineNumber $ (lines content)) (Board f [])

	print $ execState (playGame (players b) ) (field b)

	hClose handle