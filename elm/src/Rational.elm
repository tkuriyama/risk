module Rational exposing (..)

import BigInt as BI

type alias Numerator = BI.BigInt
type alias Denominator = BI.BigInt
type Sign = Pos | Neg
    
type alias Rational = { num : Numerator
                      , denom : Denominator
                      , sign : Sign }
    
zero = BI.fromInt 0

{- Conversions -}

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

{- Arithmetic Operators -}

getSign : Rational -> Rational -> Sign
getSign a b = if eqSign a b then a.sign
              else case pos a of
                       True -> if gte a b then Pos else Neg
                       False -> if gte b a then Pos else Neg

add : Rational -> Rational -> Rational
add a b = let (an, bn) = normalize a b
              nSum = BI.add an.num bn.num
              s = getSign a b
          in { num = nSum, denom = an.denom, sign = s} |> reduce


{- Comparison Operators -}

gt : Rational -> Rational -> Bool
gt a b = let (an, bn) = normalize a b
          in BI.gt an.num bn.num     

gte : Rational -> Rational -> Bool
gte a b = let (an, bn) = normalize a b
          in BI.gte an.num bn.num
              
lt : Rational -> Rational -> Bool
lt a b = gt b a

lte : Rational -> Rational -> Bool
lte a b = gte b a

{- Rational-Specific Utilities -}

pos : Rational -> Bool
pos r = r.sign == Pos      

eqSign : Rational -> Rational -> Bool
eqSign a b = (a.sign == Pos && b.sign == Pos) ||
                (a.sign == Neg && b.sign == Neg)

normalize : Rational -> Rational -> (Rational, Rational)
normalize a b = let d = lcm a.denom b.denom
                    n1 = BI.mul a.num (BI.div d (a.denom))
                    n2 = BI.mul b.num (BI.div d (b.denom))
                in ( { num = n1, denom = d, sign = a.sign }
                   , { num = n2, denom = d, sign = b.sign })

reduce : Rational -> Rational
reduce r = let d = gcd r.num r.denom
           in { num = BI.div r.num d
              , denom = BI.div r.denom d
              , sign = r.sign }

{- Arithmetic Utilities -}
               
lcm : BI.BigInt -> BI.BigInt -> BI.BigInt 
lcm a b = BI.div (BI.mul a b) (gcd a b)
      
gcd : BI.BigInt -> BI.BigInt -> BI.BigInt
gcd a b = if a == zero then b
          else if b == zero then a
          else case (BI.modBy b a) of
                   (Just m) -> gcd b m
                   Nothing -> zero
