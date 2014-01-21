module Mower.Core.Tests (tests) where

import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (test, Test)

import Mower.Core

tests :: Test
tests = testGroup "Mower.Core.Tests" [
        testGroup "Turn right" [
            testCase "should_face_the_east_after_turn_right_when_facing_north" should_face_the_east_after_turn_right_when_facing_north,
            testCase "should_face_the_south_after_turn_right_when_facing_east" should_face_the_south_after_turn_right_when_facing_east,
            testCase "should_face_the_west_after_turn_right_when_facing_south" should_face_the_west_after_turn_right_when_facing_south,
            testCase "should_face_the_north_after_turn_right_when_facing_west" should_face_the_north_after_turn_right_when_facing_west
         ]
    ]

--------------------------------------------------------------------------------

should_face_the_east_after_turn_right_when_facing_north = do
	let m = makeMower 0 0 North
	turnRight m @?= makeMower 0 0 East

should_face_the_south_after_turn_right_when_facing_east = do
	let m = makeMower 0 0 East
	turnRight m @?= makeMower 0 0 South

should_face_the_west_after_turn_right_when_facing_south = do
	let m = makeMower 0 0 South
	turnRight m @?= makeMower 0 0 West

should_face_the_north_after_turn_right_when_facing_west = do
	let m = makeMower 0 0 West
	turnRight m @?= makeMower 0 0 North