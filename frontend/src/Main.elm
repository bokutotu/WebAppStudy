module Main exposing (..)

import Browser
import Html exposing (Html)

import ToDo exposing (ToDoModel, ToDoMsg, initToDo, updateToDo, viewToDo)

import Calendar

-- MAIN
main: Program () Model Msg
main =
    Browser.document { init = \_ -> init, update = update, view = view, subscriptions = always Sub.none}


-- Model
type Msg
    = ToDoMsg ToDoMsg
    | CalendarMsg Calendar.Msg


type alias Model =
    { todo : ToDoModel 
    , calendar : Calendar.Model
    }



-- init
init : (Model, Cmd Msg)
init =
    let 
        todoModel = initToDo
        calendarInit = Calendar.init
        calendarModel = Tuple.first calendarInit
        calenadrCmdMsg = Tuple.second calendarInit
        model = Model todoModel calendarModel
        cmd = Cmd.map (\msg -> CalendarMsg msg) calenadrCmdMsg
    in 
        ( model, cmd )

-- update
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToDoMsg todomsg ->
            ( { model | todo = updateToDo todomsg model.todo }, Cmd.none )
        CalendarMsg calendarmsg ->
            let 
                calendarUpdate = Calendar.update calendarmsg model.calendar
                calendarModel = Tuple.first calendarUpdate
                calendarMsg = Tuple.second calendarUpdate
                    |> Cmd.map (\msg_ -> CalendarMsg msg_)
            in 
                ( { model | calendar = calendarModel }, calendarMsg )


-- view
view : Model -> Browser.Document Msg
view model =
    let 
        todoHtml = Html.map ToDoMsg (viewToDo model.todo)
        calendarHtml = Html.map CalendarMsg (Calendar.view model.calendar)
        listHtml = [todoHtml, calendarHtml]
    in
        Browser.Document "ToDO App" listHtml
    -- Html.map ToDoMsg (viewToDo model.todo)
