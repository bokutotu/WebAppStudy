module Main exposing (..)

import Html exposing (input, form, Html, li, text, ul, div, button)
import Html.Attributes exposing (type_, value, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)
import Browser
import Dict exposing (Dict)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- Msg
type Msg = 
  Add 
  | Done 
  | Input String

-- Model
type alias Model = { todos: List String, addToDo: String }

-- init
init: Model 
init =
  { todos = [], addToDo = "" }


-- update 
update: Msg -> Model -> Model
update msg model =
  case msg of 
    Add -> 
      { model | todos = model.todos ++ [model.addToDo], addToDo = "" }
    Done -> 
      { model | addToDo = "" }
    Input toDoName ->
      { model | addToDo = toDoName }


-- view
view: Model -> Html Msg
view model = 
  div[] 
  [
    viewInputToDo model 
    , viewAllToDo model
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

viewAllToDo: Model -> Html msg
viewAllToDo model =
  model.todos
    |> List.map (li [] << List.singleton << text)
    |> ul []
