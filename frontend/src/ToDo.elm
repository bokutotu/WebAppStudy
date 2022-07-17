module ToDo exposing ( initToDo, ToDoModel, ToDoMsg, viewToDo, updateToDo )

import Html exposing ( input, form, Html, li, text, ul, div, button, select, option )
import Html.Attributes exposing (type_, value, placeholder)
import Html.Events exposing ( onClick, onInput, onSubmit, on )
import Http
import Json.Decode exposing (Decoder, map, field, int, string)




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



-- ToDoMsg
type ToDoMsg = 
  Add 
  | TurnDone 
  | Input String
  | ChangeState State Int




-- ToDoModel
type alias ToDoModel = { numToDos: Int, todos: List ToDo, addToDo: String }



-- init
initToDo: ToDoModel 
initToDo =
  { numToDos = 0, todos = [], addToDo = "" }


updateToDoItem: List ToDo -> Int -> State -> List ToDo
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
updateToDo: ToDoMsg -> ToDoModel -> ToDoModel
updateToDo msg model =
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
        todos = updateToDoItem model.todos id state
      }

-- view
viewToDo: ToDoModel -> Html ToDoMsg
viewToDo model = 
  div[] 
  [ viewInputToDo model 
  , viewToDoItems model Will
  , viewToDoItems model Doing
  , viewToDoItems model Done
  ]



viewInputToDo: ToDoModel -> Html ToDoMsg
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


viewToDoItems: ToDoModel -> State -> Html ToDoMsg
viewToDoItems model state =
  div [] 
  [ div [] [ text ( stateToString state ) ]
  , model.todos
      |> List.filter (\item -> item.state == state)
      |> List.map ( viewToDoItem >> List.singleton >> li [] )
      |> ul []
  ]
 

viewToDoItem: ToDo -> Html ToDoMsg
viewToDoItem todo =
  div []
  [ div [] [ text todo.name ]
  , div [] [ text (stateToString todo.state) ]
  , div [] [ radioButton todo.id ]
  ]



radioButton: Int -> Html ToDoMsg
radioButton id =
  select [] 
  [ option [ onClick (ChangeState Done  id) ] [ text  "Done" ]
  , option [ onClick (ChangeState Doing id) ] [ text  "Doing" ]
  , option [ onClick (ChangeState Will  id) ] [ text  "Will" ]
  ] 
