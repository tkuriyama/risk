module Rational exposing (..)

import BigInt as BI

type alias Numerator = BI.BigInt
type alias Denominator = BI.BigInt

type Rational = Rational { num : Numerator
                         , denom : Denominator }
              | NotRational String

fromInt : Int -> Int -> Rational
fromInt n d = Rational { num = BI.fromInt n, denom = BI.fromInt d }          
