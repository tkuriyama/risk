module Risk exposing (..)
import Rational as R

type alias DieValue = Int
type alias Army = Int
type alias Losses = (Army, Army)    
type alias Battlefield = (Army, Army)

type alias Probability = R.Rational

risk : String
risk = ""

