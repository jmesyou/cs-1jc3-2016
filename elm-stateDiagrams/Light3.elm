module Light3 exposing (..)

import StateDiagrams exposing (..)

import GraphicSVG exposing (..)
import List exposing (map)


--- define the state type, including parameterized types

type OnOff = On Int | Off

--- define transition functions

plus x = case x of
           Off  -> On 1
           On 1 -> On 2
           On 2 -> On 3
           _    -> On 3

off x = case x of
          _    -> Off

--- make a list of the states and where to draw them

states = [ ( (On 1), (0,0) )
         , ( (On 2), (0,60) )
         , ( (On 3), (0,120) )
         , ( Off,    (0,-60) )]

--- define the transitions you will use

transitions = [( off, "off",
                 ((Off, (60,-65)) :: (map turnOff [1..3]))
               )
              ,( plus, "+",
                 ([(Off, (-10,-30))
                  ,(On 3, (-60,125))]
                  ++ (map increment [1..2]) )
               )
              ]
increment i = (On i, (-10,-30 + 60*toFloat i))
turnOff i =   (On i, (25+15 * toFloat i,-37 + 15 * toFloat i))

-- now you can show any node in an active state

view t = collage 500 800
           [ scale 2 (viewStateDiagram states transitions (Just Off) (Just (Off,"+")))]

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

