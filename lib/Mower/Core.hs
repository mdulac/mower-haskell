{-# LANGUAGE TypeFamilies #-}

module Mower.Core (
      turnLeft
    , turnRight
    , forward
    , forwardIfTargetPositionIsValid
    , computeCommand
    , computeCommands
    , playGame
    , makeBoard
    , isValidPosition
    ) where

import System.IO()
import Data.Monoid
import Data.List()
import Data.Either()
import Control.Monad.State
import Mower.Types
import Mower.Factory()
import Mower.Parser

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

-- Guards
isValidPosition :: Position -> Field -> Bool
isValidPosition pos@(Position (x, y)) f@(Field _ mowers)
	| x < 0 = False
	| y < 0 = False
	| pos > pos' = False
	| elem pos (map ( \m -> position m ) mowers ) = False
	| otherwise = True
	where pos' = corner f

forwardIfTargetPositionIsValid :: Field -> Mower -> Mower
forwardIfTargetPositionIsValid f m = do
	let m' = forward m
	let p' = position m'
	if isValidPosition p' f then m' else m

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

--

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
