module Submarine exposing (..)

import StateDiagrams exposing (..)

import GraphicSVG exposing (..)


type Hatch = Open | Closed

type alias Submarine = (Hatch,Int)

open (hatch,depth) = (Open,depth)
close (_,depth) = (Closed,depth)

dive : Submarine -> Submarine
dive (hatch,depth) = (hatch,depth+1)

surface : Submarine -> Submarine
surface (hatch,depth) = (hatch,depth-1)

--- make a list of the states and where to draw them

-- Note that you can define as many states to draw as you want,
-- which is useful if you only want part of the diagram.
-- If states are to close together, you need to move the (x,y) positions.

states = [  ((Open,3), (-70,120))
         ,  ((Open,2), (-70, 60))
         ,  ((Open,1), (-70,  0))
         ,  ((Open,0), (-70,-60))
         ,  ((Closed,3), (70,120))
         ,  ((Closed,2), (70, 60))
         ,  ((Closed,1), (70,  0))
         ,  ((Closed,0), (70,-60))
         ]

--- define the transitions you will use

-- Here you define the transitions to draw, one kind at a time,
-- which corresponds to one ELM function at a time.
-- You specifiy the starting state for the transition, and the
-- location of the label on the arrow.  The library figures out
-- how to link the starting and ending states while going through
-- the label, but it doesn't guarantee that arrows don't cross or
-- labels/states overlap.  You need to move the (x,y) points to
-- make it readable.

-- Note:  sometimes transition functions are too long, so you can
-- use a short-form as the string.  For example, you could use "+"
-- for the function addTwoThingsTogether.

transitions = [( dive, "dive",
                 [((Open,0), (-90,-30))
                 ,((Open,1), (-90,30))
                 ,((Open,2), (-90,90))
                 ,((Closed,0), (100,-30))
                 ,((Closed,1), (100,30))
                 ,((Closed,2), (100,90))
                 ])
              ,(  surface, "surface",
                 [((Open,1), (-30,-25))
                 ,((Open,2), (-30,35))
                 ,((Open,3), (-30,95))
                 ,((Closed,1), (20,-35))
                 ,((Closed,2), (20,25))
                 ,((Closed,3), (20,85))
                 ])
              ,(  open, "open",
                 [((Open,0), (-130,-55))
                 ,((Open,1), (-130,5))
                 ,((Open,2), (-130,65))
                 ,((Open,3), (-130,125))
                 ,((Closed,0), (0,-50))
                 ,((Closed,1), (0,10))
                 ,((Closed,2), (0,70))
                 ,((Closed,3), (0,130))
                 ])
              ,(  close, "close",
                 [((Open,0), (0,-70))
                 ,((Open,1), (0,-10))
                 ,((Open,2), (0,50))
                 ,((Open,3), (0,110))
                 ,((Closed,0), (140,-55))
                 ,((Closed,1), (140,5))
                 ,((Closed,2), (140,65))
                 ,((Closed,3), (140,125))
                 ])
              ]

-- now you can show any node in an active state

view t = collage 768 600
         [ scale 2 (viewStateDiagram states transitions (Just (Open,0)) (Just ((Open,0),"dive")))]

type Msg = Tick Float GetKeyState

main = gameApp Tick {
                        model = { t = 0 }
                    ,   view = view
                    ,   update = update
                    }

-- getKeyState is a function that takes a key and tells you the state it's in:
    --JustDown is when the key has just been pressed (lasts for 1 frame after the key is pressed; it always shows for 1 frame)
    --Down is when the key is contiuing to be held down
    --JustUp is when the key was just released (lasts for 1 frame after the key is pressed; it always shows for 1 frame)
    --Up is when the key is not being pressed.

--p1 represents the arrow keys and p2 is WASD.
    --p1 and p2 are tuples where you have (x,y).
        --a positive x value is the right arrow key (or D) and a negative one is the left arrow key (or A).
        --a positive y value is the up arrow key (or W) and a negative one is the down arrow key (or S).
        --a 0 in the x or y either means no keys are being pressed in that direction, or both are.

update msg model = case msg of
    Tick t (getKeyState,p1,p2) -> let (x1,y1) = p1
                                      (x2,y2) = p2
                                  in { model | t = t }

