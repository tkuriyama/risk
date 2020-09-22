module Risk exposing (..)
import Rational as R

type alias DieValue = Int
type alias Army = Int
type alias Losses = (Army, Army)    
type alias Battlefield = (Army, Army)

type alias Probability = R.Rational


maxTroops : Battlefield -> Battlefield
maxTroops (a, b) = (min 3 (a-1), min 2 b)

updateField : Battlefield -> Losses -> Battlefield
updateField (a, b) (al, bl) = (a - al, b - bl)

applyPair : (a -> b) -> (a, a) -> (b, b)
applyPair f (a, b) = (f a, f b)            
                              
                              
risk : String
risk = ""

