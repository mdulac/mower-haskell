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
-- TODO

turnRight :: Mower -> Mower
-- TODO

forward :: Mower -> Mower
-- TODO

isValidPosition :: Position -> Field -> Bool
-- TODO

forwardIfTargetPositionIsValid :: Field -> Mower -> Mower
-- TODO

computeCommand :: Command -> Field -> Mower -> Mower
-- TODO

computeCommands :: [Command] -> Field -> State Mower ()
-- TODO

playGame :: [Player] -> State Field ()
-- TODO

makeBoard :: [(Int, String)] -> State Board ()
-- TODO