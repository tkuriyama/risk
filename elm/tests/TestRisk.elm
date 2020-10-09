module TestRisk exposing (testMaxTroops, testUpdateField, testLosses,
                          testThrow, testPLosses, testAgg, testPAWin)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import Rational exposing (..)
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
        
testThrow : Test
testThrow =
    describe "Test throw"
        [ test "single die" <|
              \_ -> throw 1 |> Expect.equal [[1], [2], [3], [4], [5], [6]]
        , test "double dice" <|
              \_ -> throw 2 |> List.length |> Expect.equal 36
        ]

testPLosses : Test
testPLosses =
    describe "Test pLosses"
        [ test "pLosses 2 2" <|
              \_ -> pLosses (throw 2) (throw 2)
                    |> Expect.equal [ (fromInt 581 1296, (2,0))
                                    , (fromInt 35 108, (1,1))
                                    , (fromInt 295 1296, (0,2)) 
                                    ]
        ]

t = Node (Rational.fromInt 1 2) [(Node (Rational.fromInt 1 4) [])]
    
testAgg : Test
testAgg =
    describe "Test basic PTree aggregation"
        [ test "1/2 * 1/4 = 1/8" <|
              \_ -> agg t |> Expect.equal (Rational.fromInt 1 8)
        ]

testPAWin : Test
testPAWin =
    describe "Test pAWin" 
        [ test "Test case for (5,5)" <|
              \_ -> pAWin (5,5) (genDict (5,5)) |> agg |> Rational.toString
                   |> Expect.equal "1311121206180325 % 3656158440062976"
        ]
