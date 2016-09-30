import GraphicSVG exposing(..)
import String exposing (concat)

-- example of a symbolic calculation:  calculating derivatives

-- to capture the data for a new calculation, we make some types

-- what is in an expression, numbers, variables, multiplication, division, exponents, sine cosine
-- these expressions they also have (), which we might call nesting, this is the data analogue of
-- Divide and Conquer

type Expr = Const Float  -- represent constant expressions by a floating-point approximation
          | Var Char
          | Mult Expr Expr
          | Div Expr Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Exponent Expr Expr
          | Sin Expr
          | Cos Expr
          
-- we need a function to display our expressions in the "normal" way

display : Expr -> String
display expr = case
                 Const c      -> toString c 
                 Var v        -> toString v
                 Mult x y     -> concat [(display x), " * ", (display y)]
                   -- divide!    combine   conquer    combine   conquer
                 Div x y
                 Add x y
                 Sub x y
                 Exponent x y
                 Sin x
                 Cos x
                
          
example1 = Exponent (Var 'x') (Const 2)
example2 = Mult (Var 'x') (Var 'x')     

-- this is the main function
-- it calls gameApp, which takes two inputs
-- Tick - a function, a special function called a Constructor
-- {...} is a record, in a record we have fields with names
-- to change something in a record 
--   {  nameOfTheRecord | nameOfTheField = newValue }
-- to get something out of record use 
--   nameOfRecord.nameOfField
main = gameApp Tick {
                        model = init    -- init is the value in the field model
                    ,   view = view
                    ,   update = update
                    }

-- this is a type, we call it Msg because it is the type of messages we handle
-- Tick, Red and Blue are called constructors or constructor functions
-- each of these messages is a transition, even though they are in a data type
type Msg = Tick Float GetKeyState
         | Red   -- these are the messages we care about
         | Blue
         | Orange

-- this defines a record called init, which is the initial state of the game
-- it must contain all the information which parametrizes our state
init = { t = 0          -- it knows the time
       , pos = (0,0)    -- it knows the position
       , clr = yellow   -- it knows a colour
       }

-- this is a function which takes the model (another word for state) and draws a view of it
view model = collage 500 500 [
                               text (toString example2) |> size 20
                                            |> filled black
                                            |> move (-18,-80)
                             ]
-- update takes messages and performs transitions
update msg model = case msg of
    Tick t (getKeyState,p1,p2) -> let (x1,y1) = p1
                                      (x2,y2) = p2
                                  in { model | t = t }
    Red -> let (x,y) = model.pos
           in { model | pos = (x+10,y), clr = red }
    Blue -> let (x,y) = model.pos
            in { model | pos = (x-10,y), clr = blue }
    Orange -> { model | pos = (0,0) }

thing model = group [ rect 100 50 |> filled model.clr
                    , circle 20 |> filled red |> addOutline (solid 1) white |> move (-25,0) |> notifyTap Red
                    , circle 20 |> filled blue |> addOutline (solid 1) white|> move (25,0) |> notifyTap Blue
                    ]
