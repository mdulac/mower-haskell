module Mower.Core.Tests (tests) where

import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (test, Test)

import Data.Maybe
import Control.Monad.State

import Mower.Core

tests :: Test
tests = testGroup "Mower.Core.Tests" [

        testGroup "makePosition" [
            testCase "should_make_position_with_positive_value" should_make_position_with_positive_value,
            testCase "should_not_make_position_with_negative_x" should_not_make_position_with_negative_x,
            testCase "should_not_make_position_with_negative_y" should_not_make_position_with_negative_y,
            testCase "should_not_make_position_with_negative_x_and_y" should_not_make_position_with_negative_x_and_y
        ],
        testGroup "makeEmptyField" [
            testCase "should_make_empty_field_if_position_is_valid" should_make_empty_field_if_position_is_valid,
            testCase "should_not_make_empty_field_if_position_is_invalid" should_not_make_empty_field_if_position_is_invalid
        ],
        testGroup "makeMower" [
            testCase "should_make_mower_if_position_is_valid" should_make_mower_if_position_is_valid,
            testCase "should_not_make_mower_if_position_is_invalid" should_not_make_mower_if_position_is_invalid
        ],
        testGroup "makePlayer" [
            testCase "should_make_player_if_commands_are_all_defined" should_make_player_if_commands_are_all_defined,
            testCase "should_not_make_player_if_commands_are_not_all_defined" should_not_make_player_if_commands_are_not_all_defined
        ],
        testGroup "toCommand" [
            testCase "should_create_command_if_char_is_valid" should_create_command_if_char_is_valid,
            testCase "should_not_create_command_if_char_is_invalid" should_not_create_command_if_char_is_invalid
        ],
        testGroup "makeCommands" [
            testCase "should_create_commands_according_to_the_commands_sequence" should_create_commands_according_to_the_commands_sequence
        ],
        testGroup "toDirection" [
            testCase "should_create_direction_if_char_is_valid" should_create_direction_if_char_is_valid,
            testCase "should_not_create_command_if_char_is_invalid" should_not_create_command_if_char_is_invalid
        ],
        testGroup "turnRight" [
            testCase "should_face_the_east_after_turn_right_when_facing_north" should_face_the_east_after_turn_right_when_facing_north,
            testCase "should_face_the_south_after_turn_right_when_facing_east" should_face_the_south_after_turn_right_when_facing_east,
            testCase "should_face_the_west_after_turn_right_when_facing_south" should_face_the_west_after_turn_right_when_facing_south,
            testCase "should_face_the_north_after_turn_right_when_facing_west" should_face_the_north_after_turn_right_when_facing_west
        ],
        testGroup "turnLeft" [
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
        testGroup "isValidPosition" [
            testCase "should_return_true_if_the_position_is_in_the_field_and_empty" should_return_true_if_the_position_is_in_the_field_and_empty,
            testCase "should_return_false_if_the_position_is_not_in_the_field" should_return_false_if_the_position_is_not_in_the_field,
            testCase "should_return_false_if_the_position_is_in_the_field_but_not_empty" should_return_false_if_the_position_is_in_the_field_but_not_empty
        ],
        testGroup "forwardIfTargetPositionIsValid" [
            testCase "should_forward_if_target_position_is_valid" should_forward_if_target_position_is_valid,
            testCase "should_not_forward_if_target_position_is_outside_the_field" should_not_forward_if_target_position_is_outside_the_field,
            testCase "should_not_forward_if_target_position_is_not_empty" should_not_forward_if_target_position_is_not_empty
        ],
        testGroup "computeCommand" [
            testCase "should_forward_if_command_is_F" should_forward_if_command_is_F,
            testCase "should_turn_left_if_command_is_L" should_turn_left_if_command_is_L,
            testCase "should_turn_right_if_command_is_R" should_turn_right_if_command_is_R
        ],
        testGroup "computeCommands" [
            testCase "should_go_back_at_the_same_place_when_execute_return" should_go_back_at_the_same_place_when_execute_return,
            testCase "should_go_back_at_the_same_place_when_execute_square_commands" should_go_back_at_the_same_place_when_execute_square_commands
        ],
        testGroup "Parser" [
         	testCase "should_parse_valid_field" should_parse_valid_field,
         	testCase "should_parse_valid_player" should_parse_valid_player,
         	testCase "should_not_parse_invalid_field" should_not_parse_invalid_field,
         	testCase "should_not_parse_invalid_player" should_not_parse_invalid_player
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

--------------------------------------------------------------------------------

should_transform_valid_string_to_commands = sequence (map toCommand "AAAGGGDDD") @?= Just [F, F, F, L, L, L, R, R, R]
should_not_transform_invalid_string_to_commands = sequence (map toCommand "AAAGGGEDD") @?= Nothing
should_transform_valid_string_to_directions = sequence (map toDirection "NNWWSSEE") @?= Just [North, North, West, West, South, South, East, East]
should_not_transform_invalid_string_to_directions = sequence (map toDirection "NNWWSSIEE") @?= Nothing

--------------------------------------------------------------------------------

should_parse_valid_field = parseField "10 10" @?= makeEmptyField 10 10
should_parse_valid_player = parsePlayer "2 2 N AGD" @?= Just ( Player (Mower (Position (2, 2)) North) [F, L, R] )
should_not_parse_invalid_field = parseField "A 10" @?= Nothing
should_not_parse_invalid_player = parsePlayer "A 2 N AGD" @?= Nothing

--------------------------------------------------------------------------------

should_make_empty_field_if_position_is_valid = makeEmptyField 2 2 @?= Just (Field (Position (2, 2)) [])

should_not_make_empty_field_if_position_is_invalid = do
    makeEmptyField (-1) 4 @?= Nothing
    makeEmptyField 0 (-4) @?= Nothing

--------------------------------------------------------------------------------

should_make_mower_if_position_is_valid = makeMower 1 5 North @?= Just ( Mower (Position (1, 5)) North )

should_not_make_mower_if_position_is_invalid = do
    makeMower (-5) 0 South @?= Nothing
    makeMower 65 (-2) West @?= Nothing 

--------------------------------------------------------------------------------

should_make_player_if_commands_are_all_defined = do
    let m = fromJust (makeMower 1 2 North)
    makePlayer m (fromJust $ makeCommands "AGD") @?= Just ( (Player m) [F, L, R] )

should_not_make_player_if_commands_are_not_all_defined =
    let p = do
        m <- makeMower 1 2 North
        c <- makeCommands "AGE"
        makePlayer m c
    in p @?= Nothing

--------------------------------------------------------------------------------

should_create_command_if_char_is_valid = do
    toCommand 'A' @?= Just F
    toCommand 'G' @?= Just L
    toCommand 'D' @?= Just R

should_not_create_command_if_char_is_invalid = toCommand 'C' @?= Nothing

--------------------------------------------------------------------------------

should_create_direction_if_char_is_valid = do
    toDirection 'N' @?= Just North
    toDirection 'E' @?= Just East
    toDirection 'S' @?= Just South
    toDirection 'W' @?= Just West

should_not_create_direction_if_char_is_invalid = toDirection 'F' @?= Nothing

--------------------------------------------------------------------------------

should_create_commands_according_to_the_commands_sequence = do
    makeCommands "AGD" @?= Just [F, L, R]
    makeCommands "AEGD" @?= Nothing

--------------------------------------------------------------------------------

should_return_true_if_the_position_is_in_the_field_and_empty =
    let valid = do
        f <- makeEmptyField 5 5
        p <- makePosition 1 1
        return $ isValidPosition p f
    in valid @?= return True

should_return_false_if_the_position_is_not_in_the_field =
    let valid = do
        f <- makeEmptyField 4 4
        p <- makePosition 5 5
        return $ isValidPosition p f
    in valid @?= return False

should_return_false_if_the_position_is_in_the_field_but_not_empty = isValidPosition (fromJust $ makePosition 1 1) ( Field (fromJust $ makePosition 5 5) [Mower (Position (1, 1)) North]) @?= False

--------------------------------------------------------------------------------

should_forward_if_target_position_is_valid = 
    let m = do
        f <- makeEmptyField 5 5
        m <- makeMower 1 1 North
        return $ forwardIfTargetPositionIsValid f m
    in m @?= makeMower 2 1 North

should_not_forward_if_target_position_is_outside_the_field =
    let m = do
        f <- makeEmptyField 5 5
        m <- makeMower 5 1 North
        return $ forwardIfTargetPositionIsValid f m
    in m @?= makeMower 5 1 North

should_not_forward_if_target_position_is_not_empty =
    let m = do
        p <- makePosition 5 5
        m' <- makeMower 2 2 North
        f <- Just $ Field p [m']
        m <- makeMower 1 2 North
        return $ forwardIfTargetPositionIsValid f m
    in m @?= makeMower 1 2 North

--------------------------------------------------------------------------------

should_forward_if_command_is_F =
    let m = do
        f <- makeEmptyField 5 5
        m <- makeMower 1 1 North
        return $ computeCommand F f m
    in m @?= makeMower 2 1 North

should_turn_left_if_command_is_L =
    let m = do
        f <- makeEmptyField 5 5
        m <- makeMower 1 1 North
        return $ computeCommand L f m
    in m @?= makeMower 1 1 West

should_turn_right_if_command_is_R =
    let m = do
        f <- makeEmptyField 5 5
        m <- makeMower 1 1 North
        return $ computeCommand R f m
    in m @?= makeMower 1 1 East

--------------------------------------------------------------------------------

should_go_back_at_the_same_place_when_execute_return = do
    let state = computeCommands [F, F, F, F, L, L, F, F, F, F] ( fromJust (makeEmptyField 10 10) )
    let mower = execState state (fromJust $ makeMower 2 2 North)
    mower @?= fromJust (makeMower 2 2 South)

should_go_back_at_the_same_place_when_execute_square_commands = do
    let state = computeCommands [F, R, F, R, F, R, F, R] ( fromJust (makeEmptyField 5 5) )
    let mower = execState state (fromJust $ makeMower 2 2 North)
    mower @?= fromJust (makeMower 2 2 North)

