module ToDo exposing (Model, Msg, init, update, view)

import Html exposing (Html, button, div, form, input, li, option, select, text, ul)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onClick, onInput, onSubmit)


type State
    = Will
    | Done
    | Doing


type alias ToDo =
    { id : Int
    , name : String
    , state : State
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


newToDo : Int -> String -> ToDo
newToDo id name =
    { id = id, name = name, state = Will }



-- ToDoMsg


type Msg
    = Add
    | TurnDone
    | Input String
    | ChangeState State Int



-- ToDoModel


type alias Model =
    { numToDos : Int, todos : List ToDo, addToDo : String }



-- init


init : Model
init =
    { numToDos = 0, todos = [], addToDo = "" }


updateToDoItem : List ToDo -> Int -> State -> List ToDo
updateToDoItem todoList id state =
    let
        toggle item =
            if item.id == id then
                { item | state = state }

            else
                item
    in
    List.map toggle todoList



-- update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add ->
            { model
                | todos = model.todos ++ [ newToDo model.numToDos model.addToDo ]
                , addToDo = ""
                , numToDos = model.numToDos + 1
            }

        TurnDone ->
            { model | addToDo = "" }

        Input toDoName ->
            { model | addToDo = toDoName }

        ChangeState state id ->
            { model
                | todos = updateToDoItem model.todos id state
            }



-- view


view : Model -> Html Msg
view model =
    div []
        [ viewInputToDo model
        , viewToDoItems model Will
        , viewToDoItems model Doing
        , viewToDoItems model Done
        ]


viewInputToDo : Model -> Html Msg
viewInputToDo model =
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


viewToDoItems : Model -> State -> Html Msg
viewToDoItems model state =
    div []
        [ div [] [ text (stateToString state) ]
        , model.todos
            |> List.filter (\item -> item.state == state)
            |> List.map (viewToDoItem >> List.singleton >> li [])
            |> ul []
        ]


viewToDoItem : ToDo -> Html Msg
viewToDoItem todo =
    div []
        [ div [] [ text todo.name ]
        , div [] [ text (stateToString todo.state) ]
        , div [] [ radioButton todo.id ]
        ]


radioButton : Int -> Html Msg
radioButton id =
    select []
        [ option [ onClick (ChangeState Done id) ] [ text "Done" ]
        , option [ onClick (ChangeState Doing id) ] [ text "Doing" ]
        , option [ onClick (ChangeState Will id) ] [ text "Will" ]
        ]
