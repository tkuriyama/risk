module Main exposing (main)

-- import Rational exposing (..)
import Show as Show
import Show2 as Show2
import Risk exposing (..)

--import Browser.Events exposing (onKeyPress)
-- import String exposing (fromInt)

import Browser exposing (element)
import Html exposing (Html, div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import TypedSvg exposing (svg)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Opacity(..))
import TypedSvg.Attributes exposing (viewBox, fillOpacity)

fst : (a, b) -> a
fst (a, _) = a
             
snd : (a, b) -> b
snd (_, d) = d
           
initModel : Model            
initModel = ((5, 5), genDict (10, 10))

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.none)

view : Model -> Html Msg
view (b, dict) =
    div []
        [ button [ onClick DecrementA ] [ text "-" ]
        , text ("A: " ++ String.fromInt (fst b))
        , button [ onClick IncrementA ] [ text "+" ]
        , text (" " )
        , button [ onClick DecrementD ] [ text "-" ]
        , text ("B: " ++ String.fromInt (snd b))
        , button [ onClick IncrementD ] [ text "+" ]            
        , svg [ viewBox 0 0 1200 1400
              , fillOpacity <| Opacity 1 ]
              ((Show.render 0 0 1200 500 (b, dict)) ++
               (Show2.render 0 500 1200 500 (b, dict)))
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ((a, d), dict) =
    case msg of
        IncrementA -> (((if a < 10 then a+1 else a, d), dict), Cmd.none)
        DecrementA -> (((if a > 2 then a-1 else a, d), dict), Cmd.none)
        IncrementD -> (((a, if d < 10 then d+1 else d), dict), Cmd.none)
        DecrementD -> (((a, if d > 1 then d-1 else d), dict), Cmd.none)

-- subscriptions : Model -> Sub Msg
-- subscriptions model =
--    onKeyPress keyDecoder
           
main = element 
    { init = init
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }

