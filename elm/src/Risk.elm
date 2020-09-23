module Risk exposing (..)
import Rational as R
import Dict exposing (Dict)

type alias DieValue = Int
type alias Army = Int
type alias Losses = (Army, Army)    
type alias Battlefield = (Army, Army)

type alias Probability = R.Rational
type alias Scenario = (Probability, Losses)

type PTree = Node Probability (List PTree)

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

rLength : List a -> R.Rational
rLength l = let len = List.length l
            in R.fromInt len 1
                
group : List a -> List (List a)
group xs =
    let f x (cur, acc) =
            case cur of
                [] -> ([x], acc)
                (y::ys) -> if x == y then ((x::y::ys), acc)
                           else ([x], (y::ys)::acc)
    in List.foldl f ([], []) xs |> (\(last, acc) -> last::acc)

crossMap : List a -> List b -> (a -> b -> c) -> List c
crossMap xs ys f =
    xs |> List.concatMap (\x -> List.map (\y -> f x y) ys)
    
pLosses : List (List DieValue) -> List (List DieValue) -> List Scenario
pLosses xss yss =
    let pairs = crossMap xss yss (\a b -> losses (a, b))
        groups = List.sort pairs |> group
        total = rLength pairs
        genPair g = case List.head g of
                        (Just p) -> (R.div (rLength g) total, p)
                        Nothing -> (R.div (rLength g) total, (0,0))
    in List.map genPair groups

pDict : Dict Battlefield (List Scenario)
pDict = let f a b = ((a, b), pLosses (throw a) (throw b))
        in crossMap [1,2,3] [1,2] f |> Dict.fromList
        
