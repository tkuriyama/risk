module Risk exposing (..)
import Rational as R
import Dict exposing (Dict)

type alias DieValue = Int
type alias Army = Int
type alias Losses = (Army, Army)    
type alias Battlefield = (Army, Army)

type alias Probability = R.Rational
type alias Scenario = (Probability, Losses)

{- Utilities -}
    
maxTroops : Battlefield -> Battlefield
maxTroops (a, d) = (min 3 (a-1), min 2 d)

updateField : Battlefield -> Losses -> Battlefield
updateField (a, d) (al, dl) = (a - al, d - dl)

applyPair : (a -> b) -> (a, a) -> (b, b)
applyPair f (a, b) = (f a, f b)            

losses : (List DieValue, List DieValue) -> Losses
losses pair =
    let (atts, defs) = applyPair (List.reverse << List.sort) pair
        ls = List.map2 (\a d -> if d >= a then 1 else 0) atts defs
    in (List.sum ls, List.length ls - List.sum ls)

{- Probabilities -}        

throw : Int -> List (List DieValue)
throw n =
    case n of           
        0 -> [[]]
        i -> List.range 1 6
             |> List.concatMap (\d -> throw (i-1)
                                      |> List.map ((::) d))



