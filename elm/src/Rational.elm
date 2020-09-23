{-
Rational numbers, built on top of BigInt.
Numerators and Denominators are always stored as positive BigInts, with a sign.
-}

module Rational exposing (fromInt, fromBigInt, toString,
                          add, sub, mul, div,
                          gt, gte, lt, lte, negate, absolute,
                          eqSign, gcd, lcm, Rational)

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
    in { num = BI.abs <| BI.fromInt n
       , denom = BI.abs <| BI.fromInt d
       , sign = s}  

fromBigInt : BI.BigInt -> BI.BigInt -> Rational
fromBigInt n d =
    let s = if (BI.gte n zero && BI.gte d zero) || (BI.lt n zero && BI.lt d zero)
            then Pos else Neg
    in { num = BI.abs n
       , denom = BI.abs d
       , sign = s }

toString : Rational -> String
toString r = BI.toString r.num ++ " % " ++ BI.toString r.denom           
        
{- Arithmetic Operators -}

-- assumes inputs are already normalized
addEqSign : Rational -> Rational -> Rational
addEqSign a b =
    let s = if pos a then Pos else Neg
    in { num = BI.add a.num b.num
       , denom = a.denom
       , sign = s }    

-- assumes inputs are already normalized        
addNotEqSign : Rational -> Rational -> Rational
addNotEqSign a b = 
    let (x, y) = if pos a then (a, b) else (b, a)
        s = if BI.gte x.num y.num then Pos else Neg
    in { num = BI.sub x.num y.num |> BI.abs
       , denom = x.denom
       , sign = s }
                                                                    
add : Rational -> Rational -> Rational
add a b = let (an, bn) = normalize a b                         
              f = if eqSign a b then addEqSign else addNotEqSign
          in f an bn |> reduce

sub : Rational -> Rational -> Rational
sub a b = add a (negate b)

mulSign : Rational -> Rational -> Sign
mulSign a b = if eqSign a b then Pos else Neg
    
mul : Rational -> Rational -> Rational
mul a b = let s = mulSign a b
              n = BI.mul a.num b.num
              d = BI.mul a.denom b.denom
          in { num = n, denom = d, sign = s } |> reduce          

div : Rational -> Rational -> Rational
div a b = mul a { num = b.denom, denom = b.num, sign = b.sign }

{- Comparison Operators -}

gt : Rational -> Rational -> Bool
gt a b = case eqSign a b of
             True -> let (an, bn) = normalize a b
                     in if pos a then BI.gt an.num bn.num
                         else BI.gt bn.num an.num
             False -> pos a

gte : Rational -> Rational -> Bool
gte a b = gt a b || (reduce a) == (reduce b)
              
lt : Rational -> Rational -> Bool
lt a b = gt b a

lte : Rational -> Rational -> Bool
lte a b = gte b a

{- Rational-Specific Utilities -}

pos : Rational -> Bool
pos r = r.sign == Pos      

negate : Rational -> Rational
negate r = let s = if pos r then Neg else Pos
           in { num = r.num, denom = r.denom, sign = s }

absolute : Rational -> Rational               
absolute r = if pos r then r else negate r
        
eqSign : Rational -> Rational -> Bool
eqSign a b = (a.sign == Pos && b.sign == Pos) ||
             (a.sign == Neg && b.sign == Neg)

-- normalize to common demoninator, do not reduce                 
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

-- least common multiple
lcm : BI.BigInt -> BI.BigInt -> BI.BigInt 
lcm a b = BI.div (BI.mul a b) (gcd a b)

-- greater common divisor          
gcd : BI.BigInt -> BI.BigInt -> BI.BigInt
gcd a b = if a == zero then b
          else if b == zero then a
          else case (BI.modBy b a) of
                   (Just m) -> gcd b m
                   Nothing -> zero
