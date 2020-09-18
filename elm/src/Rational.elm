module Rational exposing (..)

import BigInt as BI

type alias Numerator = BI.BigInt
type alias Denominator = BI.BigInt

type Rational = Rational { num : Numerator
                         , denom : Denominator }
              | NotRational String

fromInt : Int -> Int -> Rational
fromInt n d = Rational { num = BI.fromInt n, denom = BI.fromInt d }          

fromBigInt : BI.BigInt -> BI.BigInt -> Rational
fromBigInt n d = Rational { num = n, denom = d }

-- gcd :: BigInt -> BigInt -> BigInt
-- gcd                  
                 
-- function gcd(a, b)
--     while a ≠ b 
--         if a > b
--             a := a − b
--         else
--             b := b − a
--     return a      
