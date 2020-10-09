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
        s2 = R.toInt (R.mul r (R.fromInt 100 1)) |> \i -> (String.fromInt i) ++ "%"
        s = s1 ++ "  |  (" ++ s2 ++ ")"
        sWidth = String.length s * 9 |> toFloat
    in text_ [ x (px <| w /2  - sWidth / 2)
             , y (px <| textY)
             , class ["infoText"]
             ]
             [ text s ]

{- Show Tree -}

aggLevel : R.Rational -> List PTree -> Float
aggLevel p0 ts = let f (Node p _) acc = R.add p acc -- (R.mul p0 p) acc
                 in List.foldr f (R.fromInt 0 1) ts |> R.toFloatN 5          

showLeaf : R.Rational -> List PTree -> Float -> Float -> Float -> Float ->
           List (Svg msg)
showLeaf p ts xStart xEnd yStart yInc =
    let pNotLose = if ts == [] then 1.0 else aggLevel p ts
        xOffset = (xEnd - xStart) * 0.1
        xLen = xEnd - xStart - (xOffset * 2)
        xLenW = xLen * pNotLose
        xLenL = xLen - xLenW
    in [ rect [ x (px <| xStart + xOffset)
              , y (px yStart)
              , width (px <| xLenW)
              , height (px <| yInc * 0.5)
              , rx (px 2)
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
                     
showBranch : Float -> Float -> Float -> Float -> PTree -> List (Svg msg)
showBranch xStart xEnd yStart yInc (Node p ts) =
    let len = List.length ts
        xInc = (xEnd - xStart) / (toFloat len)
        yStart2 = yStart + yInc
        xStart2s = List.range 1 len
                   |> List.map (\i -> xStart + xInc * ((toFloat i)-1))
        f t xStart2 = showBranch xStart2 (xStart2 + xInc) (yStart + yInc) yInc t
    in (showLeaf p ts xStart xEnd yStart yInc) ++
        (List.map2 f ts xStart2s |> List.concat)
                 
showTree : Float -> Float -> PTree -> List (Svg msg)
showTree w h t =
    let depth = maxDepth t
        yStart = 25
        yInc = min 100 ((h - textY - yStart) / (toFloat depth))
    in showBranch 0 w (textY + yStart) yInc t        

{- Render Main -}        
                 
render : Float -> Float -> Model -> List (Svg msg)
render w h m = let t = pAWin m
               in [ showText w h t ] ++ showTree w h t
