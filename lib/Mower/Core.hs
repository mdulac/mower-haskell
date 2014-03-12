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
turnLeft = undefined

turnRight :: Mower -> Mower
turnRight = undefined

forward :: Mower -> Mower
forward = undefined

isValidPosition :: Position -> Field -> Bool
isValidPosition = undefined

forwardIfTargetPositionIsValid :: Field -> Mower -> Mower
forwardIfTargetPositionIsValid = undefined

computeCommand :: Command -> Field -> Mower -> Mower
computeCommand = undefined

computeCommands :: [Command] -> Field -> State Mower ()
computeCommands = undefined

playGame :: [Player] -> State Field ()
playGame = undefined

makeBoard :: [(Int, String)] -> State Board ()
makeBoard  = undefined