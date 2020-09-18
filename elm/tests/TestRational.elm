module TestRational exposing (testBigInt, testFrom, testGCD, testLCM)

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
        , test "zero right" <|
              \_ -> gcd one zero |> Expect.equal one
        ,  test "252 105 -> 21" <|
              \_ -> gcd (BI.fromInt 105) (BI.fromInt 252)
                    |> Expect.equal (BI.fromInt 21)
    ,  test "order invaraince" <|
              \_ -> gcd (BI.fromInt 105) (BI.fromInt 252)
                    |> Expect.equal (gcd (BI.fromInt 252) (BI.fromInt 105))     
    ]

testLCM: Test
testLCM =
    describe "Test LCM"
        [ test "zero left" <|
              \_ -> lcm zero one |> Expect.equal zero
        , test "zero right" <|
              \_ -> lcm one zero |> Expect.equal zero
        ,  test "21 6 -> 42" <|
              \_ -> lcm (BI.fromInt 21) (BI.fromInt 6)
                    |> Expect.equal (BI.fromInt 42)
    ]
