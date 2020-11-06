-- Original Tree Representation
-- Game-State Focused     
    
module Show exposing (..)

import Risk exposing (..)
import Rational as R

import Color
import String

import TypedSvg exposing (circle, svg, rect, line, text_)
import TypedSvg.Attributes exposing (x, y, x1, y1, x2, y2, cx, cy, fill, r, rx,
                                     stroke, strokeWidth, opacity, class,
                                     fillOpacity, width, height, viewBox)
import TypedSvg.Types exposing (Paint(..), px, Opacity(..))
import TypedSvg.Core exposing (Svg, text)

-- All rendering starts from (rX, rY) coordinates

{- Show Text -}

textY = 18

showText : Float -> Float -> Float -> Float -> PTree -> Svg msg
showText rX rY w h t =
    let r = agg t
        s1 = R.toString r
        s2 = R.toFloatN 1 (R.mul r (R.fromInt 100 1))
             |> \i -> (String.fromFloat i) ++ "%"
        s = s1 ++ "  |  (" ++ s2 ++ ")"
        sWidth = String.length s * 8 |> toFloat
    in text_ [ x (px <| w/2 - sWidth/2)
             , y (px <| rY + textY)
             , class ["infoText"]
             ]
             [ text s ]

{- Show Tree -}

aggLevel : R.Rational -> List PTree -> Float
aggLevel p0 t =
    case t of
        [] -> 1.0
        ts -> let f (Node p _) acc = R.add p acc
              in List.foldr f (R.fromInt 0 1) ts |> R.toFloatN 5

showLeaf : R.Rational -> List PTree -> Float -> Float -> Float -> Float ->
           List (Svg msg)
showLeaf p ts xStart xEnd yStart yInc =
    let pNotLose = aggLevel p ts
        xOffset = (xEnd - xStart) / 2 - (min 20 ((xEnd - xStart) * 0.25))
        xLen = (min 40 ((xEnd - xStart) * 0.75))
        xLenW = xLen * pNotLose
        xLenL = xLen - xLenW
    in [ rect [ x (px <| xStart + xOffset)
              , y (px yStart)
              , width (px <| xLenW)
              , height (px <| yInc * 0.5)
              , rx (px 4)
              , strokeWidth (px 1)
              , stroke <| Paint <| Color.lightGrey
              , fill <| Paint <| Color.lightGreen
              , opacity <| Opacity 0.5
              ]
              []
       , rect [ x (px <| xStart + xOffset + xLenW)
              , y (px yStart)
              , width (px <| xLenL)
              , height (px <| yInc * 0.5)
              , rx (px 2)
              , strokeWidth (px 1)
              , stroke <| Paint <| Color.lightGrey
              , fill <| Paint <| Color.lightRed
              , opacity <| Opacity 0.5
              ]
              []
       ]

isLeaf : List PTree -> Bool
isLeaf t =
    case t of
        [] -> True
        ((Node _ ts) :: ns) -> ns == [] && ts == []

getLeafXs : Float -> Float -> Int -> List (Float, Float)
getLeafXs start w n =
    let inc = w / (toFloat n)
    in List.range 1 n
      |> List.map toFloat
      |> List.map (\i -> (start + (i-1)*inc, start + i * inc))

genLines : Float -> Float -> Float -> Float -> List (Float, Float) ->
           List (Svg msg)
genLines xStart xEnd yStart yInc pairs =
    let xMid1 = (xStart + xEnd) / 2
        genLine (xStart2, xEnd2) =
            line [ x1 (px xMid1)
                 , x2 (px <| (xStart2 + xEnd2) / 2)
                 , y1 (px <| yStart + yInc * 0.5)
                 , y2 (px <| yStart + yInc)
                 , strokeWidth (px 1.5)
                 , stroke <| Paint <| Color.black
                 ]
                 []
    in List.map genLine pairs

showBranch : Float -> Float -> Float -> Float -> PTree -> List (Svg msg)
showBranch xStart xEnd yStart yInc (Node p ts) =
    let len = List.length ts
        xPairs = getLeafXs xStart (xEnd - xStart) (List.length ts)
        f t (xStart2, xEnd2) = showBranch xStart2 xEnd2 (yStart + yInc) yInc t
    in (showLeaf p ts xStart xEnd yStart yInc) ++
       case isLeaf ts of
           True -> []
           _ -> (List.map2 f ts xPairs |> List.concat) ++
                genLines xStart xEnd yStart yInc xPairs

showTree : Float -> Float -> Float -> Float -> PTree -> List (Svg msg)
showTree rX rY w h t =
    let depth = maxDepth t
        yOffset = 25
        yStart = rY + yOffset
        yInc = min 40 ((h - textY - yOffset) / (toFloat depth))
    in showBranch rX w (textY + yStart) yInc t

{- Render Main -}

render : Float -> Float -> Float -> Float -> Model -> List (Svg msg)
render rX rY w h (b, dict) =
    let t = pAWin b dict
    in [ showText rX rY w h t ] ++ showTree rX rY w h t

