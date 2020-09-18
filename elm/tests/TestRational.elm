module TestRational exposing (testBigInt, testFrom)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import BigInt as BI
import Rational exposing (..)

testBigInt : Test
testBigInt =
    describe "Basic BigInt"
        [ test "Identity" <|
              \_ -> BI.fromIntString "1234"
                    |> Expect.equal (BI.fromIntString "1234")
        , test "Addition" <|
            \_ -> BI.add (BI.fromInt 999) (BI.fromInt 1)
                  |> Expect.equal (BI.fromInt 1000) ]

testFrom : Test
testFrom =
    describe "Test fromInt and fromBigInt"
        [ test "equivalence" <|
              \_ -> fromInt 1000 2000
                    |> Expect.equal (fromBigInt (BI.fromInt 1000) (BI.fromInt 2000))]

zero = BI.fromInt 0
one = BI.fromInt 1
      
testGCD : Test
testGCD =
    describe "Test GCD"
        [ test "zero left" <|
              \_ -> gcd zero one |> Expect.equal one
              ]
