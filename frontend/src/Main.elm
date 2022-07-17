module Main exposing (..)

import Html exposing ( Html )
import Browser

import ToDo exposing ( initToDo, updateToDo, ToDoModel, ToDoMsg, viewToDo )



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }


-- Model



type Msg = ToDoMsg ToDoMsg


type alias Model = { todo: ToDoModel }


-- init 



init: Model
init =
  { todo = initToDo }



-- update


update: Msg -> Model -> Model
update msg model =
  case msg of
    ToDoMsg todomsg ->
      { model | todo = ( updateToDo todomsg model.todo ) }



-- view


view: Model -> Html Msg
view model =
  Html.map ToDoMsg ( viewToDo model.todo ) 

