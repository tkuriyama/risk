module Main exposing (main)

-- import Rational exposing (..)
import Show exposing (..)
import Risk exposing (..)

--import Browser.Events exposing (onKeyPress)
-- import String exposing (fromInt)

import Browser exposing (element)
import Html exposing (Html, div, text, button)
import Html.Events exposing (onClick)
import TypedSvg exposing (svg)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Attributes exposing (viewBox)

fst : (a, b) -> a
fst (a, _) = a
             
snd : (a, b) -> b
snd (_, d) = d
           
initModel : Model            
initModel = (5, 5)

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick DecrementA ] [ text "-" ]
        , text ("A: " ++ String.fromInt (fst model))
        , button [ onClick IncrementA ] [ text "+" ]
        , button [ onClick DecrementD ] [ text "-" ]
        , text ("B: " ++ String.fromInt (snd model))
        , button [ onClick IncrementD ] [ text "+" ]            
--        , svg [ viewBox 0 0 1100 800 ] (render 750 750 m) 
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg (a, d) =
    case msg of
        IncrementA -> ((a+1, d), Cmd.none)
        DecrementA -> if a > 2 then ((a-1, d), Cmd.none) else ((a, d), Cmd.none)
        IncrementD -> ((a, d+1), Cmd.none)
        DecrementD -> if d > 2 then ((a, d-1), Cmd.none) else ((a, d), Cmd.none)

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--    onKeyPress keyDecoder
           
main = element 
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

