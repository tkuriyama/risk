module TestRisk exposing (testMaxTroops, testUpdateField, testLosses)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Risk exposing (..)

testMaxTroops : Test
testMaxTroops =
    describe "Test Max Troops for basic cases "
        [ test "never exceeds" <|
              \_ -> maxTroops (5, 5) |> Expect.equal (3,2)
        , test "floor" <|
              \_ -> maxTroops (1, 1) |> Expect.equal (0,1)
        ]

testUpdateField : Test
testUpdateField =
    describe "Test updateField"
        [ test "simple update" <|
              \_ -> updateField (3,2) (2,1) |> Expect.equal (1,1)
        ]
        
testLosses : Test
testLosses =
    describe "Test losses"
        [ test "same length" <|
              \_ -> losses ([3,2], [2,2]) |> Expect.equal (1,1)
        , test "diff length" <|
              \_ -> losses ([3,2,1], [2,2]) |> Expect.equal (1,1)
        ]
        
