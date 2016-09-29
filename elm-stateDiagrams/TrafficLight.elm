import Signal
import Window
import List
import Graphics.Collage exposing (..)
import Time exposing (..)
import Graphics.Element exposing (Element)
import Window
import StateDiagrams exposing (..)

{-
    Using the Elm Architecture, we begin our programs 
    by creating a "Model", which is a type containing
    everything that can change in the program

    In our case, our model contains 
        1. The current State that is highlighted
        2. The current Transition that is highlighted

    "states" and "transitions" are not in the model
    because they are static and never change 
-}

type alias Model = 
    {highlightedState : Maybe State,
     highlightedTransition : Maybe (State, String)
    }

type State = Red | Yellow | Green

{-
    Now that we have defined what our Model is, we can
    make Model that the program will start in, we can 
    name this "initialModel"

    Our program will start transitioning to the Green
    state from Red
    
-}

initialModel : Model
initialModel = {
                    highlightedState = Just Green
                ,   highlightedTransition = Just (Red, "change")
                }


{-
    "states" and "transitions" contain what the StateDiagrams
    library will draw to the screen. Both of these lists contain
    something names to draw to the screen and the locations of
    where they will be drawn
-}

states = 
    [
         (Red,    (0, 200))
    ,    (Yellow, (0, 0))
    ,    (Green,  (0, -200))
    ]
transitions = 
    [
        (updateState, "change",
        [((Red),    (100,  0))
        ,((Yellow), (-100,  100))
        ,((Green),  (-100, -100))])
    ]


{- 
    Using the state diagrams library requires us to write a function
    containing the instructions for updating the state. This function
    takes a State and gives us back a new State.

    For a traffic light, this is fairly trivial.
        - A Green light turns Yellow
        - A Yellow light turns Red
        - A Red light turns Green
-}

updateState : State -> State
updateState t = case t of
    Green -> Yellow
    Yellow -> Red
    Red -> Green

{-
    In Elm, we usually have an update function with the type:
    update : a -> Model -> Model

    where "a" is any type. In this case, we are taking
    in a float (because "t" is time)

    This means that our update function has the type
    signature:
    update : Float -> Model -> Model

    The only thing we update in this program is the
    highlightedState and highlightedTransition. The function
    just applies the updateState function to 
    gs.highlightedState and gs.highlightedTransition
-}

update : Float -> Model -> Model
update t gs = {
            gs | highlightedState <- gs.highlightedState |> (\(Just a) -> Just (updateState a))
            , highlightedTransition <- gs.highlightedTransition |> (\(Just (a, c)) -> Just (updateState a, c))
            }

{-
    In Elm, we also have a view function with the type:
    view : Model -> a -> Element

    This function will contain all of the logic to draw
    the model. We always look at the state of our model
    in order to determine what to draw.

    In our case, view passes all of the draw logic to
    the viewStateDiagram function.

    viewStateDiagram is the main function
    that we call from the StateDiagrams library,

    It will give us a Form, which will be converted
    to an Element through the collage function
-}

view : Model -> Element
view gs = collage 768 760 [viewStateDiagram states transitions gs.highlightedState gs.highlightedTransition]

{-
    The main function in this program says:

    1. Apply the update function to the state every second
    2. Draw the newly updated state
-}

main : Signal Element
main = Signal.map view (Signal.foldp update initialModel (every second))