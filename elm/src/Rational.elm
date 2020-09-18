module Rational exposing (..)

import BigInt as BI

type alias Numerator = BI.BigInt
type alias Denominator = BI.BigInt

type alias Rational = { num : Numerator
                      , denom : Denominator }

zero = BI.fromInt 0

-- -------------
-- Convertions
----------------

fromInt : Int -> Int -> Rational
fromInt n d = { num = BI.fromInt n, denom = BI.fromInt d }          

fromBigInt : BI.BigInt -> BI.BigInt -> Rational
fromBigInt n d = { num = n, denom = d }

-------------
-- Arithmetic 
----------------

add : Rational -> Rational -> Rational
add a b = let d = lcm a.denom b.denom
              n1 = BI.mul a.num (BI.div d (a.denom))
              n2 = BI.mul b.num (BI.div d (b.denom))                   
          in { num = BI.add n1 n2, denom = d } |> reduce

-- -------------
-- Utilities
----------------

reduce : Rational -> Rational
reduce r = let d = gcd r.num r.denom
           in { num = BI.div r.num d, denom = BI.div r.denom d }

lcm : BI.BigInt -> BI.BigInt -> BI.BigInt 
lcm a b = BI.div (BI.mul a b) (gcd a b)
      
gcd : BI.BigInt -> BI.BigInt -> BI.BigInt
gcd a b = if a == zero then b
          else if b == zero then a
          else case (BI.modBy b a) of
                   (Just m) -> gcd b m
                   Nothing -> zero
