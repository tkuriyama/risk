module TestRisk exposing (testMaxTroops)

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
