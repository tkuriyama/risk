module Risk exposing (..)
import Rational as R

type alias DieValue = Int
type alias Army = Int
type alias Losses = (Army, Army)    
type alias Battlefield = (Army, Army)

type alias Probability = R.Rational


maxTroops : Battlefield -> Battlefield
maxTroops (a, b) = (max 3 (a-1), max 2 b)

updateField : Battlefield -> Losses -> Battlefield
updateField (a, b) (al, bl) = (a - al, b - bl)
                              
risk : String
risk = ""

