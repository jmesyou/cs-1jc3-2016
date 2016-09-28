module StateDiagrams exposing(viewStateDiagram)

import GraphicSVG exposing (..)
import List exposing(map,map2,foldr,concatMap)
import String exposing (length)
import Dict exposing (Dict)
import Array

-- make an Element showing the state diagram
viewStateDiagram states transitions activeState activeTransition
  = let
       dispStates = map displayState states
       dispTrans = concatMap displayTransition transitions
       locsSizes = Dict.fromList <| map extractStatePosSize dispStates
    in
       group (
              (List.concat <| map (drawTransition locsSizes activeTransition)
                                  dispTrans
              ) ++
              (map (drawWithActive activeState) dispStates)
             )

-- apply transition function to initial states for display
displayTransition :  (a -> a, String, List (a, (Float, Float))) -> List (a, a, (Float, Float), String)
displayTransition (tfun,label,initialStateAndPoss) =
  let mkT (state,pos) = (state,tfun state,pos,label)
  in map mkT initialStateAndPoss

type alias ViewNode a = { node : a
                        , position : {x : Float, y : Float}
                        , size : {x : Float, y : Float}
                        }

xy : {x : Float, y : Float} -> (Float, Float)
xy record = (record.x,record.y)

toXY : (Int, Int) -> {x : Float, y : Float}
toXY (x,y) = {x = toFloat x, y = toFloat y}

mkSize : (Int, Int) -> {x : Float, y : Float}
mkSize (x,y) = {x = 10 + (toFloat x), y = toFloat y}

displayState : (a, (Int, Int)) -> ViewNode a
displayState (state,pos) = {node = state, position = toXY pos, size = mkSize (sizeOf (toString state))}

extractStatePosSize : ViewNode a -> (String, ((Float, Float), (Float, Float)))
extractStatePosSize viewNode = (toString viewNode.node, (xy viewNode.position,xy viewNode.size))

drawWithActive : Maybe a -> ViewNode a -> Shape s
drawWithActive activeState viewNode
  = group ((if activeState == Just viewNode.node
               then [outlined (solid 4.0)
                              (highlight)
                              (oval viewNode.size.x viewNode.size.y)
                    ]
               else []
              )
              ++ [ outlined (solid 2.0)
                            (black)
                            (oval viewNode.size.x viewNode.size.y)
                 , move (-12.0, -4.0) (displayNode (text (toString viewNode.node)))
                 --, toForm <| show <| sizeOf <| show viewNode.node
                 ]
          )
      |> move (xy viewNode.position)

drawTransition :  Dict String ((Float, Float), (Float, Float)) -> Maybe (a, String)  -> (a, a, (Float, Float), String)  -> List (Shape s)
drawTransition locsSizes activeT (pre,post,pos,name)=
  let preS = toString pre
      postS = toString post
      prePS = Dict.get preS locsSizes
      postPS = Dict.get postS locsSizes
      width = 7 * (toFloat <| String.length name )
      --(lx,ly) = sizeOf <| text <| Text.monospace <| fromString name
      (_,ly) = sizeOf name
      isActive = activeT == Just (pre,name)
  in case (prePS,postPS) of
       (Just (prePos,preSize), Just (postPos,postSize)) ->
           [ if preS == postS
               then selfLoop prePos pos preSize isActive
               else parabola3 prePos pos postPos preSize postSize isActive
           , filled (rgba 255 255 255 0.8)
                    (rect width 12)
                          |> move pos
           , move pos (displayNode (monospace name 12)) |> move (-0.5*width,-4)
           --, show (lx,ly) |> toForm |> move pos |> move (10,10)
           ]
       otherwise    -> []

normalize : (Float, Float) -> (Float, Float)
normalize (x,y) = let r = sqrt (x*x + y*y) in (x/r,y/r)

rotateTo : (Float, Float) -> (Float, Float) -> (Float, Float)
rotateTo (x,y) (u,v) = (x*1 + y*1, x*1 - y*1)

infixl 6 -.
(-.) (x,y) (u,v) = (x-u,y-v)
infixl 6 +.
(+.) (x,y) (u,v) = (x+u,y+v)
perp : (Float, Float) -> (Float, Float)
perp (x,y) = (-y,x)
infixl 8 *.
(*.) s (x,y) = (s*x,s*y)
dot : (Float, Float) -> (Float, Float) -> Float
dot (x,y) (u,v) = x*u + y*v
absdot : (Float, Float) -> (Float, Float) -> Float
absdot (x,y) (u,v) = abs(x*u) + abs(y*v)

selfLoop : (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool -> Shape s
selfLoop (x0,y0) (x1,y1) sz0 isActive =
    let
        inter = interpxy startPt (x1,y1) endPt
        inner = map inter [2..18]
        tangent = perp <| (x1,y1) -. (x0,y0)
        (xs,ys) = normalize <| ((x1,y1) -. tangent) -. (x0,y0)
        (xe,ye) = normalize <| ((x1,y1) +. tangent) -. (x0,y0)
        endDir = normalize <| (inter 18) -. endPt
        endPt = (0.5 * xe * fst sz0,0.5 * ye * snd sz0) +. (x0,y0)
        t1 = (endPt +. (10 *. endDir)) +. (4 *. (perp endDir))
        t2 = (endPt +. (10 *. endDir)) -. (4 *. (perp endDir))
        startPt = (0.5 * xs * fst sz0,0.5 * ys * snd sz0) +. (x0,y0)
        disp = 1 *. ( endPt -. startPt )
        arc = curve startPt [Pull ((x1,y1) -. disp) (x1,y1), Pull ((x1,y1) +. disp) endPt]
        testPts = map (\ t -> let pt = (fst sz0 * cos t, snd sz0 * sin t)
                                  len = 0.5 * (pt `absdot` sz0)
                              in 0.5 *. pt              ) [0..16]
    in group <| (if isActive
                   then [ outlined (solid 2)
                                   (highlight)
                                   arc
                        , filled  (highlight)
                                  (polygon [endPt,t1,t2])
                        ]
                   else []
                ) ++ [ outlined (solid 2)
                                (black)
                                arc
                     , filled (black)
                              (polygon [endPt,t1,t2])
                ]

parabola3 : (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool -> Shape s
parabola3 (x0,y0) (x1,y1) (x2,y2) sz0 sz2 isActive
  = let
        inter = interpxy startPt (x1,y1) endPt
        (xe,ye) = normalize <| (x1,y1) -. (x2,y2)
        endDir = normalize <| (inter 18) -. endPt
        endPt = (0.5 * xe * fst sz2,0.5 * ye * snd sz2) +. (x2,y2)
        t1 = (endPt +. (10 *. endDir)) +. (4 *. (perp endDir))
        t2 = (endPt +. (10 *. endDir)) -. (4 *. (perp endDir))
        t1active = (endPt +. (12 *. endDir)) +. (6 *. (perp endDir))
        t2active = (endPt +. (12 *. endDir)) -. (6 *. (perp endDir))
        (xs,ys) = normalize <| (x1,y1) -. (x0,y0)
        startPt = (0.5 * xs * fst sz0,0.5 * ys * snd sz0) +. (x0,y0)
        -- half the vector between start and end
        disp = 0.25 *. ( endPt -. startPt )
        arc = curve startPt [Pull ((x1,y1) -. disp) (x1,y1), Pull ((x1,y1) +. disp) endPt]
        testPts = map (\ t -> let pt = (fst sz0 * cos t, snd sz0 * sin t)
                                  len = 0.5 * (pt `absdot` sz0)
                              in 0.5 *. pt              ) [0..16]

    in group <| (if isActive
                   then [ outlined (solid 4)
                                   (highlight)
                                   (arc)
                        , filled (highlight)
                                 (polygon [endPt -. 4 *. endDir,t1active,t2active])
                        ]
                   else []
                ) ++ [ outlined (solid 2)
                                (black)
                                (arc)
                     , filled (black)
                              (polygon [endPt,t1,t2])
                ]


--a*t^2 + b*t + c = x
--c = x0
--a + b + c = x1
--4*a + 2*b + c = x2
--a + b = x1 - x0
--4*a + 2*b = x2 - x0
--a = 0.5 * (x2 - x0 - 2*(x1-x0)) = 0.5 * (x2 - 2*x1 + x0)
--b = 0.5 * (-(x2 - x0) + 4*(x1-x0)) = 0.5 * (-x2 + 4*x1 - 3*x0)

interpxy : (Float, Float) -> (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
interpxy (x0,y0) (x1,y1) (x2,y2) t = (interp (x0,x1,x2) t, interp (y0,y1,y2) t)

interp : (Float, Float, Float) -> Float -> Float
interp (x0,x1,x2) t =
    let a = 0.5 * (x2 - 2*x1 + x0)
        b = 0.5 * (-x2 + 4*x1 - 3*x0)
        c = x0
    in a*0.01*t*t + b*0.1*t + c

highlight = rgb 0 191 255

monospace : String -> Float -> Stencil
monospace str sz = text str |> fixedwidth |> size sz

displayNode : Stencil -> Shape notification
displayNode shp = filled (black) shp

sizeOf : String -> (Int, Int)
sizeOf str = (8 * (length str), 16)
