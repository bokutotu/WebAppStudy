module ToDo exposing (..)

import Html.Styled.Events exposing (onClick, onSubmit, onInput)
import Html.Styled exposing (Html,div, form, input, li, option, select, text, ul, button)
import Html.Styled.Attributes as AttrHtml
import Html.Styled.Attributes exposing (type_, value, placeholder)

import Date exposing (Date)
import Time exposing (Month(..))


type State
    = Will
    | Done
    | Doing


type alias ToDo =
    { id : Int
    , name : String
    , state : State
    , date: Date
    }


stateToString : State -> String
stateToString state =
    case state of
        Will ->
            "Will"

        Done ->
            "Done"

        Doing ->
            "Doing"


newToDo : Int -> String -> Date -> ToDo
newToDo id name date =
    ToDo id name Will date


-- ToDoMsg
type Msg
    = Add
    | TurnDone
    | Input String
    | ChangeState State Int


-- ToDoModel
type alias Model =
    { numToDos : Int, todos : List ToDo, addToDo : String, date: Date }


-- init
init : Model
init =
    Model 0 [] "" (Date.fromCalendarDate 2020 Jan 1)


updateItem : List ToDo -> Int -> State -> List ToDo
updateItem todoList id state =
    let
        toggle item =
            if item.id == id then
                { item | state = state }

            else
                item
    in
    List.map toggle todoList


updateDate: Date -> Model -> Model
updateDate date_ model =
    { model | date = date_ }


-- update
update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | todos = model.todos ++ [ newToDo model.numToDos model.addToDo model.date ]
                , addToDo = ""
                , numToDos = model.numToDos + 1
            }

        TurnDone ->
            { model | addToDo = "" }

        Input toDoName ->
            { model | addToDo = toDoName }

        ChangeState state id ->
            { model
                | todos = updateItem model.todos id state
            }


-- view
view : Model -> Html Msg
view model =
    div 
        []
        [ viewInput model
        , viewItems model Will
        , viewItems model Doing
        , viewItems model Done
        ]


viewInput : Model -> Html Msg
viewInput model =
    form
        [ onSubmit Add
        ]
        [ div []
            [ input
                [ type_ "text"
                , value model.addToDo
                , placeholder "To Do Name"
                , onInput Input
                ]
                []
            , button
                [ type_ "submit"
                ]
                [ text "Add" ]
            ]
        ]


viewItems : Model -> State -> Html Msg
viewItems model state =
    div []
        [ div [] [ text (stateToString state) ]
        , model.todos
            |> List.filter (\item -> item.state == state)
            |> List.filter (\item -> item.date == model.date)
            |> List.map (viewItem >> List.singleton >> li [])
            |> ul []
        ]


viewItem : ToDo -> Html Msg
viewItem todo =
    div [ AttrHtml.class "todo_item" ]
        [ div [ AttrHtml.class "todo_name" ] [ text todo.name ]
        , div [ AttrHtml.class "todo_state" ] [ text (stateToString todo.state) ]
        , div [ AttrHtml.class "todo_radio_button" ] [ radioButton todo.id ]
        ]


radioButton : Int -> Html Msg
radioButton id =
    select []
        [ option [ onClick (ChangeState Done id) ] [ text "Done" ]
        , option [ onClick (ChangeState Doing id) ] [ text "Doing" ]
        , option [ onClick (ChangeState Will id) ] [ text "Will" ]
        ]
