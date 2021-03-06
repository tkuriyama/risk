module TestRational exposing (testBigInt, testFrom, testToInt, testToFloatN,
                              testGCD, testLCM,
                              testAdd, testSub, testMul, testDiv,
                              testComp, testEqSign)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

import BigInt as BI
import Rational exposing (..)

zero = BI.fromInt 0
one = BI.fromInt 1

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
                    |> Expect.equal (fromBigInt (BI.fromInt 1000) (BI.fromInt 2000))
        , test "zero sign" <|
              \_ -> eqSign (fromInt 0 0) (fromInt 1 1)
                    |> Expect.equal True
        , test "zero sign bigInt" <|
              \_ -> eqSign (fromBigInt zero zero) (fromBigInt one one )
                    |> Expect.equal True
        , test "negative bigint" <|
              \_ -> eqSign (fromBigInt one one) (fromBigInt one (BI.fromInt (-1)) )
                    |> Expect.equal False                                   
        ]              

testToInt : Test
testToInt =
    describe "test toInt"
        [ test "toInt 1" <|
              \_ -> fromInt 1 1 |> toInt |> Expect.equal 1
        , test "toInt 4/3 -> 1" <|
              \_ -> fromInt 4 3 |> toInt |> Expect.equal 1
        ]

testToFloatN : Test
testToFloatN =
    describe "test toFloatN"
        [ test "toFloat 1 1" <|
              \_ -> fromInt 1 1 |> toFloatN 1
                    |> Expect.within (Expect.Absolute 0.000000001) 1.0
        , test "toFloat 3 123/1000" <|
              \_ -> fromInt 123 1000 |> toFloatN 3
                    |> Expect.within (Expect.Absolute 0.000000001) 0.123
        ]

        
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

testLCM : Test
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

testAdd : Test
testAdd =
    describe "Test addition"
        [ test "0/1 + 1/2 -> 1/2" <|
              \_ -> add (fromInt 0 1) (fromInt 1 2)
                    |> Expect.equal (fromInt 1 2)
        , test "0/1 + 1/1 -> 1/1" <|
              \_ -> add (fromInt 0 1) (fromInt 1 1)
                    |> Expect.equal (fromInt 1 1)
        , test "1/1 + -1/2 -> 1/2" <|
              \_ -> add (fromInt 1 1) (fromInt (-1) 2)
                    |> Expect.equal (fromInt 1 2)
        , test "-1/1 + -1/2 -> -3/2" <|
              \_ -> add (fromInt (-1) 1) (fromInt (-1) 2)
                 |> Expect.equal (fromInt (-3) 2)  
        , test "-1/2 + 1/1 -> 1/2" <|
              \_ -> add (fromInt (-1) 2) (fromInt 1 1)
                    |> Expect.equal (fromInt 1 2)  
        , test "21/35 + 73/129 -> 752/645" <|
             \_ -> add (fromInt 21 35) (fromInt 73 129)
                   |> Expect.equal (fromInt 752 645)                          
        ]

testSub : Test
testSub =
    describe "Test subtraction"
        [ test "1/2 - 1/2 -> 0/0" <|
              \_ -> sub (fromInt 1 2) (fromInt 1 2)
                    |> Expect.equal (fromInt 0 1)
        , test "1/2 - 1 -> -1/2" <|
              \_ -> sub (fromInt 1 2) (fromInt 1 1)
                    |> Expect.equal (fromInt 1 (-2))
        , test "-1/2 - 1 -> -3/2" <|
              \_ -> sub (fromInt (-1) 2) (fromInt 1 1)
                    |> Expect.equal (fromInt (-3) 2)
        , test "-1/2 - (-1) -> 1/2" <|
              \_ -> sub (fromInt (-1) 2) (fromInt (-1) 1)
                    |> Expect.equal (fromInt 1 2)                         
        , test "arbitrary subtraction" <|
              \_ -> sub (fromInt 123 1234) (fromInt 697 12359)
                    |> Expect.equal (fromInt 38827 897118)
        ]

testMul : Test
testMul =
    describe "Test division"
        [ test "simple mul" <|
              \_ -> mul (fromInt 45 123) (fromInt 124 29)
                    |> Expect.equal (fromInt 1860 1189) 
        , test "simple mul diff signs" <|
              \_ -> mul (fromInt 45 123) (fromInt (-124) 29)
                    |> Expect.equal (fromInt 1860 (-1189))
        , test "simple mul two negatives" <|
              \_ -> mul (fromInt (-45) 123) (fromInt (-124) 29)
                    |> Expect.equal (fromInt 1860 1189)                         
        , test "mul by zero" <|
              \_ -> mul (fromInt 45 123) (fromInt 0 1)
                    |> Expect.equal (fromInt 0 1)
        ]
        
testDiv : Test
testDiv =
    describe "Test divide"
        [ test "simple div" <|
              \_ -> div (fromInt 45 123) (fromInt 124 29)
                    |> Expect.equal (fromInt 435 5084)
        , test "div by one" <|
              \_ -> div (fromInt 45 123) (fromInt 1 1)
                    |> Expect.equal (fromInt 15 41)
        ]
        
testComp : Test
testComp =
    describe "Test comparison operators (lt is implemented with gt)"
        [ test "gt same sign pos" <|
              \_ -> gt (fromInt 1 2) (fromInt 1 3) |> Expect.equal True
        , test "gt same sign neg" <|
             \_ -> gt (fromInt (-1) 3) (fromInt (-1) 2) |> Expect.equal True
        , test "gt diff sign" <|
             \_ -> gt (fromInt (-1) 3) (fromInt 1 2) |> Expect.equal False
         , test "lt" <|
              \_ -> lt (fromInt 1 3) (fromInt 1 2) |> Expect.equal True
        , test "lte pass" <|
              \_ -> lte (fromInt 1 3) (fromInt 1 2) |> Expect.equal True
        , test "lte fail" <|
              \_ -> lte (fromInt 1 2) (fromInt 1 3) |> Expect.equal False
        , test "lte equals" <|
              \_ -> lte (fromInt 2 5) (fromInt 2 5) |> Expect.equal True
        ]
        
testEqSign : Test
testEqSign =
    describe "Test eqSign"
        [ test "neg and pos -> false" <|
              \_ -> eqSign (fromInt (-1) 2) (fromInt 1 2)
                    |> Expect.equal False
        , test "abs neg and pos -> false" <|
              \_ -> eqSign (fromInt (-1) 2 |> absolute) (fromInt 1 2)
                    |> Expect.equal True         
        , test "neg and neg -> true" <|
              \_ -> eqSign (fromInt (-1) 2) (fromInt 1 (-2))
                    |> Expect.equal True  
        ]
