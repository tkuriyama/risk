module Rational exposing (..)

import BigInt as BI

type alias Numerator = BI.BigInt
type alias Denominator = BI.BigInt

type Rational = Rational { num : Numerator
                         , denom : Denominator }
              | NotRational String

-- r = Rational { numerator = BI.fromIntString "123", denominator = BI.fromIntString "456" }    
                             
