module Show exposing (..)

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

{- Show Text -}

textY = 10

showText : Float -> Float -> PTree -> Svg msg
showText w h t =
    let r = agg t
        s1 = R.toString r
        s2 = R.toFloatN 1 (R.mul r (R.fromInt 100 1)) |> \i -> (String.fromFloat i) ++ "%"
        s = s1 ++ "  |  (" ++ s2 ++ ")"
        sWidth = String.length s * 7 |> toFloat
    in text_ [ x (px <| w/2 - sWidth/2)
             , y (px <| textY)
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
        xOffset = (xEnd - xStart) * 0.1
        xLen = xEnd - xStart - (xOffset * 2)
        xLenW = xLen * pNotLose
        xLenL = xLen - xLenW
    in [ rect [ x (px <| xStart + xOffset)
              , y (px yStart)
              , width (px <| xLenW)
              , height (px <| yInc * 0.5)
              , rx (px 4)
              , strokeWidth (px 1.5)
              , fill <| Paint <| Color.lightGreen
              , opacity <| Opacity 0.5
              ]
              []
       , rect [ x (px <| xStart + xOffset + xLenW)
              , y (px yStart)
              , width (px <| xLenL)
              , height (px <| yInc * 0.5)
              , rx (px 2)
              , strokeWidth (px 1.5)
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
        
getLeafXs : Float -> Float -> List PTree -> List (Float, Float)
getLeafXs start w t =
    case t of
        [] -> [(start, w)]
        ((Node p _)::ts) -> let end = start + (R.mul (R.fromInt (round w) 1) p
                                               |> R.toFloatN 5)
                            in (start, end) :: getLeafXs end w ts
        
showBranch : Float -> Float -> Float -> Float -> PTree -> List (Svg msg)
showBranch xStart xEnd yStart yInc (Node p ts) =
    let len = List.length ts
        xPairs = getLeafXs xStart (xEnd - xStart) ts
        f t (xStart2, xEnd2) = showBranch xStart2 xEnd2 (yStart + yInc) yInc t
    in (showLeaf p ts xStart xEnd yStart yInc) ++
       if isLeaf ts then [] else (List.map2 f ts xPairs |> List.concat)
                 
showTree : Float -> Float -> PTree -> List (Svg msg)
showTree w h t =
    let depth = maxDepth t
        yStart = 25
        yInc = min 50 ((h - textY - yStart) / (toFloat depth))
    in showBranch 0 w (textY + yStart) yInc t        

{- Render Main -}        
                 
render : Float -> Float -> Model -> List (Svg msg)
render w h (b, dict) = let t = pAWin b dict
               in [ showText w h t ] ++ showTree w h t

                   
