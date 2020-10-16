-- Revised Tree Representation
-- Probability-Space Oriented    

module Show2 exposing (..)

import Risk exposing (..)
import Rational as R

import Color
import String

import TypedSvg exposing (circle, svg, rect, line, text_)
import TypedSvg.Attributes exposing (x, y, x1, y1, x2, y2, cx, cy, fill, r, rx,
                                     stroke, strokeWidth, opacity, class, fillOpacity,
                                     width, height, viewBox)
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

splitX : Float -> Float -> Float -> (Float, Float, Float)
splitX xStart xEnd p = ( xEnd - xStart
                       , (xEnd - xStart) * p
                       , (xEnd - xStart) * (1-p))
                  
showRect : Float -> Float -> Float -> Float -> Float -> Color.Color -> Float ->
           Svg msg
showRect xStart yStart w h r c o = 
    rect [ x (px xStart)
         , y (px yStart)
         , width (px w)
         , height (px h)
         , rx (px r)
         , fill <| Paint <| c
         , opacity <| Opacity o
         , strokeWidth (px 0.5)--(px 1.1)
         , stroke <| Paint Color.lightGrey --Color.darkGrey                  
         ] []

-- Left == Undecided or Win; Right == Loss

showLeaf : Float -> Float -> Float -> Float -> Float -> Bool -> Int -> List (Svg msg)
showLeaf pNotLose xStart xEnd yStart yInc endLeaf d =
    let (_, xLenL, xLenR) = splitX xStart xEnd pNotLose
        leftFill = if endLeaf then Color.lightGreen else Color.white
        left = showRect xStart yStart xLenL (yInc * 0.5) 4 leftFill 0.5
        right = showRect (xStart+xLenL) yStart xLenR (yInc * 0.5) 4 (Color.lightRed) 0.5
        leftCarry = carryLeaves xStart xLenL yStart yInc Color.lightGreen (d-1)
        rightCarry = carryLeaves (xStart+xLenL) xLenR yStart yInc Color.lightRed (d-1)
    in [left, right] ++ (if endLeaf then leftCarry ++ rightCarry else [])

carryLeaves : Float -> Float -> Float -> Float -> Color.Color -> Int -> List (Svg msg)
carryLeaves xStart xLen yStart yInc c d =
    List.range 1 d
    |> List.map toFloat
    |> List.map (\i -> yStart + yInc * i)
    |> List.map (\yStart2 ->
                     showRect xStart yStart2 xLen (yInc*0.5) 4 c 0.5)
        
isLeaf : List PTree -> Bool         
isLeaf t =
    case t of
        [] -> True
        ((Node _ ts) :: ns) -> ns == [] && ts == []

extractPs : List PTree -> List Float
extractPs ts = let unwrap (Node p _) = R.toFloatN 4 p
               in List.map unwrap ts
                               
getLeafXs : Float -> Float -> List PTree -> List (Float, Float)
getLeafXs start w ts =
    let f n (s, pairs) = (s+n, ((s, s+n) :: pairs))
    in extractPs ts
       |> List.map (\p -> p * w)
       |> List.foldl f (start, [])
       |> \(a, b) -> b

genLines : Float -> Float -> Float -> Float -> List (Float, Float) -> List (Svg msg)
genLines xStart xEnd yStart yInc pairs =
    let xMid1 = (xStart + xEnd) / 2
        genLine (xStart2, xEnd2) =
            line [ x1 (px xMid1)
                 , x2 (px <| (xStart2 + xEnd2) / 2)
                 , y1 (px <| yStart + yInc * 0.5)
                 , y2 (px <| yStart + yInc)
                 , strokeWidth (px 1.5)
                 , stroke <| Paint <| Color.darkGrey
                 ]
                 [] 
    in List.map genLine pairs
        
showBranch : Float -> Float -> Float -> Float -> PTree -> Int -> List (Svg msg)
showBranch xStart xEnd yStart yInc (Node p ts) d =
    let pNotLose = aggLevel p ts
        (xLen, xLenL, xLenR) = splitX xStart xEnd pNotLose
        xPairs = getLeafXs xStart xLen ts
        f t (xStart2, xEnd2) = showBranch xStart2 xEnd2 (yStart + yInc) yInc t (d-1)
    in (showLeaf pNotLose xStart xEnd yStart yInc (isLeaf ts) d) ++
       case isLeaf ts of
           True -> []
           _ -> (List.map2 f ts xPairs |> List.concat) ++
                (genLines xStart xEnd yStart yInc xPairs) ++
                carryLeaves (xStart+xLenL) xLenR yStart yInc Color.lightRed (d-1)
                 
showTree : Float -> Float -> Float -> Float -> PTree -> List (Svg msg)
showTree rX rY w h t =
    let depth = maxDepth t
        yOffset = 25
        yStart = rY + yOffset
        yInc = min 40 ((h - textY - yOffset) / (toFloat depth))
    in showBranch rX w (textY + yStart) yInc t (depth-1)

{- Render Main -}        
                 
render : Float -> Float -> Float -> Float -> Model -> List (Svg msg)
render rX rY w h (b, dict) =
    let t = pAWin b dict
    in [ showText rX rY w h t ] ++ showTree rX rY w h t

                   
