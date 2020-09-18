module Rational exposing (..)

import BigInt as BI

type alias Numerator = BI.BigInt
type alias Denominator = BI.BigInt

type Rational = Rational { num : Numerator
                         , denom : Denominator }
              | NotRational String

zero = BI.fromInt 0
                
fromInt : Int -> Int -> Rational
fromInt n d = Rational { num = BI.fromInt n, denom = BI.fromInt d }          

fromBigInt : BI.BigInt -> BI.BigInt -> Rational
fromBigInt n d = Rational { num = n, denom = d }
                 
gcd : BI.BigInt -> BI.BigInt -> BI.BigInt
gcd a b = if b == zero then a
          else case (BI.modBy a b) of
                   (Just m) -> gcd b m
                   Nothing -> zero

-- function gcd(a, b)
--    if b = 0
--        return a
--    else
--        return gcd(b, a mod b)                 

