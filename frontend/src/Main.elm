module Main exposing (..)

import Browser
-- import Html exposing (..)
import Html.Styled exposing (map, toUnstyled)

import ToDo

import Calendar

-- MAIN
main: Program () Model Msg
main =
    Browser.document { init = \_ -> init, update = update, view = view, subscriptions = always Sub.none}


-- Model
type Msg
    = ToDoMsg ToDo.Msg
    | CalendarMsg Calendar.Msg


type alias Model =
    { todo : ToDo.Model 
    , calendar : Calendar.Model
    }



-- init
init : (Model, Cmd Msg)
init =
    let 
        todoModel = ToDo.init
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
            ( { model | todo = ToDo.update todomsg model.todo }, Cmd.none )
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
        calendarHtml = map CalendarMsg (Calendar.view model.calendar)
        listHtml = List.singleton (toUnstyled calendarHtml)
    in
        Browser.Document "ToDo App" listHtml
