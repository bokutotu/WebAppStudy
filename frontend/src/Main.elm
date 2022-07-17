module Main exposing (..)

import Html exposing ( input, form, Html, li, text, ul, div, button, select, option )
import Html.Attributes exposing (type_, value, placeholder)
import Html.Events exposing ( onClick, onInput, onSubmit, on )
import Http
import Json.Decode exposing (Decoder, map, field, int, string)
import Browser



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


type State = 
  Will
  | Done
  | Doing

type alias ToDo = 
  { id: Int
  , name : String
  , state: State
  }


stateToString: State -> String
stateToString state =
  case state of
    Will -> 
      "Will"
    Done -> 
      "Done"
    Doing ->
      "Doing"


newToDo: Int -> String -> ToDo
newToDo id name = 
  {id = id, name = name, state = Will}



-- Msg
type Msg = 
  Add 
  | TurnDone 
  | Input String
  | ChangeState State Int




-- Model
type alias Model = { numToDos: Int, todos: List ToDo, addToDo: String }



-- init
init: Model 
init =
  { numToDos = 0, todos = [], addToDo = "" }


updateToDo: List ToDo -> Int -> State -> List ToDo
updateToDo todoList id state =
  let
    toggle item =
      if item.id == id then
        { item | state = state }
      else
        item
  in
    List.map toggle todoList


-- update 
update: Msg -> Model -> Model
update msg model =
  case msg of 
    Add -> 
      { model | 
        todos = model.todos ++ [ newToDo model.numToDos model.addToDo]
        , addToDo = ""
        , numToDos = model.numToDos + 1
      }
    TurnDone -> 
      { model | addToDo = "" }
    Input toDoName ->
      { model | addToDo = toDoName }
    ChangeState state id ->
      { model |
        todos = updateToDo model.todos id state
      }

-- view
view: Model -> Html Msg
view model = 
  div[] 
  [ viewInputToDo model 
  , viewToDos model Will "Will"
  , viewToDos model Doing "Doing"
  , viewToDos model Done "Done"
  ]



viewInputToDo: Model -> Html Msg
viewInputToDo model  =
  form [
    onSubmit Add
  ]
  [
    div []
    [
      input 
      [ type_ "text" 
      , value model.addToDo
      , placeholder "To Do Name"
      , onInput Input
      ] []
      , button
      [
        type_ "submit"
      ] [text "Add"]
    ]
  ]


viewToDos: Model -> State -> String -> Html Msg
viewToDos model state div_title =
  div [] [
   div [] [ text div_title ]
   , model.todos
      |> List.filter (\item -> item.state == state)
      |> List.map ( viewToDoItem >> List.singleton >> li [] )
      |> ul []
  ]
 

viewToDoItem: ToDo -> Html Msg
viewToDoItem todo =
  div []
  [ div [] [ text todo.name ]
  , div [] [ text (stateToString todo.state) ]
  , div [] [ radioButton todo.id ]
  ]



radioButton: Int -> Html Msg
radioButton id =
  select [] 
  [ option [ onClick (ChangeState Done  id) ] [ text  "Done" ]
  , option [ onClick (ChangeState Doing id) ] [ text  "Doing" ]
  , option [ onClick (ChangeState Will  id) ] [ text  "Will" ]
  ] 
