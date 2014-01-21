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
         ],
        testGroup "Turn left" [
            testCase "should_face_the_west_after_turn_left_when_facing_north" should_face_the_west_after_turn_left_when_facing_north,
            testCase "should_face_the_north_after_turn_left_when_facing_east" should_face_the_north_after_turn_left_when_facing_east,
            testCase "should_face_the_east_after_turn_left_when_facing_south" should_face_the_east_after_turn_left_when_facing_south,
            testCase "should_face_the_south_after_turn_left_when_facing_west" should_face_the_south_after_turn_left_when_facing_west
         ],
        testGroup "forward" [
        	testCase "should_forward_to_the_north" should_forward_to_the_north,
        	testCase "should_forward_to_the_east" should_forward_to_the_east,
        	testCase "should_forward_to_the_south" should_forward_to_the_south,
        	testCase "should_forward_to_the_west" should_forward_to_the_west
         ]
    ]

--------------------------------------------------------------------------------

should_face_the_east_after_turn_right_when_facing_north = let m = makeMower 0 0 North in turnRight m @?= makeMower 0 0 East
should_face_the_south_after_turn_right_when_facing_east = let m = makeMower 0 0 East in turnRight m @?= makeMower 0 0 South
should_face_the_west_after_turn_right_when_facing_south = let m = makeMower 0 0 South in turnRight m @?= makeMower 0 0 West
should_face_the_north_after_turn_right_when_facing_west = let m = makeMower 0 0 West in turnRight m @?= makeMower 0 0 North

--------------------------------------------------------------------------------

should_face_the_west_after_turn_left_when_facing_north = let m = makeMower 0 0 North in turnLeft m @?= makeMower 0 0 West
should_face_the_north_after_turn_left_when_facing_east = let m = makeMower 0 0 East in turnLeft m @?= makeMower 0 0 North
should_face_the_east_after_turn_left_when_facing_south = let m = makeMower 0 0 South in turnLeft m @?= makeMower 0 0 East
should_face_the_south_after_turn_left_when_facing_west = let m = makeMower 0 0 West in turnLeft m @?= makeMower 0 0 South

--------------------------------------------------------------------------------

should_forward_to_the_north = let m = makeMower 0 0 North in forward m @?= makeMower 1 0 North
should_forward_to_the_east = let m = makeMower 0 0 East in forward m @?= makeMower 0 1 East
should_forward_to_the_south = let m = makeMower 0 0 South in forward m @?= makeMower (-1) 0 South
should_forward_to_the_west = let m = makeMower 0 0 West in forward m @?= makeMower 0 (-1) West

--------------------------------------------------------------------------------

