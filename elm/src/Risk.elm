module Risk exposing (..)
import Rational as R
import Dict exposing (Dict)

{- Model / Msg Types -}

type alias Model = (Battlefield, Dict Battlefield PTree)

type Msg = IncrementA | DecrementA
         | IncrementD | DecrementD
           
{- Risk Types -} 

type alias DieValue = Int
type alias Army = Int
type alias Losses = (Army, Army)    
type alias Battlefield = (Army, Army)

type alias Probability = R.Rational
type alias Scenario = (Probability, Losses)

type PTree = Node Probability (List PTree)

{- Tree Utilities -}

intMax : List Int -> Int
intMax xs = case List.maximum xs of
                 Just x -> x
                 Nothing -> 0

maxDepth : PTree -> Int
maxDepth (Node p ts) = 
    case ts of
        [] -> 1
        xs -> 1 + (List.map maxDepth xs |> intMax)

    
{- Risk Utilities -}
    
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
        
agg : PTree -> Probability
agg (Node p ts) =
    case ts of
        [] -> p
        xs -> let f t acc = R.add acc (R.mul p (agg t))
              in List.foldr f (R.fromInt 0 1) xs

nodeZero = Node (R.fromInt 0 1) []
           
aLoses : Battlefield -> Scenario -> Bool
aLoses (a, d) (_, (aLoss, dLoss)) =
    if d - dLoss <= 0 then False else a - aLoss <= 1

{- Memoized Probabilities -}

updateHead : Probability -> PTree -> PTree
updateHead p0 (Node p ts) = Node p0 ts

genTreeMemo : Battlefield -> Probability -> Dict Battlefield PTree -> PTree
genTreeMemo b p0 dict =
    case (b, Dict.get b dict) of
        ((_, 0), _) -> Node p0 []
        ((1, _), _) -> nodeZero
        (_, (Just pt)) -> updateHead p0 pt
        (_, _) ->
            case Dict.get (maxTroops b) pDict of
                Nothing -> nodeZero
                Just pairs ->
                    let update (p, ls) = (p, updateField b ls)
                        branch (p, bNext) = genTreeMemo bNext p dict
                    in List.filter (not << aLoses b) pairs
                       |> List.map update
                       |> List.map branch
                       |> Node p0
          
seedDict : Dict Battlefield PTree
seedDict = Dict.fromList [ ((1, 0), Node (R.fromInt 1 0) [])
                         , ((0, 1), nodeZero)
                         , ((1, 1), nodeZero) ]

genDict : Battlefield -> Dict Battlefield PTree 
genDict (a, d) =
    let atts = List.range 1 a
        defs = List.range 1 d
        pairs = crossMap atts defs (\att def -> (att, def))
        f b dict = let t = genTreeMemo b (R.fromInt 1 1) dict
                   in Dict.insert b t dict
    in List.foldl f seedDict pairs

pAWin b dict = case Dict.get b dict of
                   Just t -> t
                   Nothing -> Node (R.fromInt 0 1) []
                             
                                
