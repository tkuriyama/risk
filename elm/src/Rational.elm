module Rational exposing (..)

import BigInt as BI

type alias Numerator = BI.BigInt
type alias Denominator = BI.BigInt
type Sign = Pos | Neg
    
type alias Rational = { num : Numerator
                      , denom : Denominator
                      , sign : Sign }
    
zero = BI.fromInt 0

-- -------------
-- Convertions
----------------

fromInt : Int -> Int -> Rational
fromInt n d =
    let s = if (n >= 0 && d >= 0) || (n < 0 && d < 0)
            then Pos else Neg
    in { num = BI.fromInt n, denom = BI.fromInt d, sign = s}  

fromBigInt : BI.BigInt -> BI.BigInt -> Rational
fromBigInt n d =
    let s = if (BI.gte n zero && BI.gte d zero) || (BI.lt n zero && BI.lt d zero)
            then Pos else Neg
    in { num = n, denom = d, sign = s }

-------------
-- Arithmetic 
----------------

add : Rational -> Rational -> Rational
add a b = let d = lcm a.denom b.denom
              n1 = BI.mul a.num (BI.div d (a.denom))
              n2 = BI.mul b.num (BI.div d (b.denom))
              nSum = BI.add n1 n2
              s = if signMatch a b then a.sign
                  else if gte a b then Pos else Neg
          in { num = BI.add n1 n2, denom = d, sign = s} |> reduce


-- -------------
-- Comparisons
----------------

gte : Rational -> Rational -> Bool
gte a b = let (an, bn) = normalize a b
          in BI.gte an.num bn.num
              
-- -------------
-- Utilities
----------------

normalize : Rational -> Rational -> (Rational, Rational)
normalize a b = let d = lcm a.denom b.denom
                    n1 = BI.mul a.num (BI.div d (a.denom))
                    n2 = BI.mul b.num (BI.div d (b.denom))
                in ( { num = n1, denom = d, sign = a.sign }
                   , { num = n2, denom = d, sign = b.sign })
    
signMatch : Rational -> Rational -> Bool
signMatch a b = (a.sign == Pos && b.sign == Pos) ||
                (a.sign == Neg && b.sign == Neg)

reduce : Rational -> Rational
reduce r = let d = gcd r.num r.denom
           in { num = BI.div r.num d
              , denom = BI.div r.denom d
              , sign = r.sign }

lcm : BI.BigInt -> BI.BigInt -> BI.BigInt 
lcm a b = BI.div (BI.mul a b) (gcd a b)
      
gcd : BI.BigInt -> BI.BigInt -> BI.BigInt
gcd a b = if a == zero then b
          else if b == zero then a
          else case (BI.modBy b a) of
                   (Just m) -> gcd b m
                   Nothing -> zero
