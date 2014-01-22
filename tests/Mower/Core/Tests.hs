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
         ],
         testGroup "Make Position" [
         	testCase "should_make_position_with_positive_value" should_make_position_with_positive_value,
         	testCase "should_not_make_position_with_negative_x" should_not_make_position_with_negative_x,
         	testCase "should_not_make_position_with_negative_y" should_not_make_position_with_negative_y,
         	testCase "should_not_make_position_with_negative_x_and_y" should_not_make_position_with_negative_x_and_y
         ]
    ]

--------------------------------------------------------------------------------

should_face_the_east_after_turn_right_when_facing_north = fmap turnRight (makeMower 0 0 North) @?= makeMower 0 0 East
should_face_the_south_after_turn_right_when_facing_east = fmap turnRight (makeMower 0 0 East) @?= makeMower 0 0 South
should_face_the_west_after_turn_right_when_facing_south = fmap turnRight (makeMower 0 0 South) @?= makeMower 0 0 West
should_face_the_north_after_turn_right_when_facing_west = fmap turnRight (makeMower 0 0 West) @?= makeMower 0 0 North

--------------------------------------------------------------------------------

should_face_the_west_after_turn_left_when_facing_north = fmap turnLeft (makeMower 0 0 North) @?= makeMower 0 0 West
should_face_the_north_after_turn_left_when_facing_east = fmap turnLeft (makeMower 0 0 East) @?= makeMower 0 0 North
should_face_the_east_after_turn_left_when_facing_south = fmap turnLeft (makeMower 0 0 South) @?= makeMower 0 0 East
should_face_the_south_after_turn_left_when_facing_west = fmap turnLeft (makeMower 0 0 West) @?= makeMower 0 0 South

--------------------------------------------------------------------------------

should_forward_to_the_north = fmap forward (makeMower 1 1 North) @?= makeMower 2 1 North
should_forward_to_the_east = fmap forward (makeMower 1 1 East) @?= makeMower 1 2 East
should_forward_to_the_south = fmap forward (makeMower 1 1 South) @?= makeMower 0 1 South
should_forward_to_the_west = fmap forward (makeMower 1 1 West) @?= makeMower 1 0 West

--------------------------------------------------------------------------------

should_make_position_with_positive_value = makePosition 0 0 @?= Just ( Position (0, 0) )
should_not_make_position_with_negative_x = makePosition (-1) 1 @?= Nothing
should_not_make_position_with_negative_y = makePosition 1 (-1) @?= Nothing
should_not_make_position_with_negative_x_and_y = makePosition (-1) (-1) @?= Nothing

