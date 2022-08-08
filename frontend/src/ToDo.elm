module ToDo exposing (..)

import Html.Styled.Events exposing (onClick, onInput)
import Html.Styled exposing 
    (Html,div, input, li, option, select, text, ul, button, textarea)
import Html.Styled.Attributes as AttrHtml
import Html.Styled.Attributes exposing (type_, value, placeholder)
import Html.Styled.Attributes exposing (css)

import Css
import Css.Global exposing (descendants, selector)

import Date exposing (Date)
import Time exposing (Month(..))


type State
    = Will
    | Done
    | Doing


type alias ToDo =
    { id : Int
    , name : String
    , content: String
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


newToDo : Int -> String -> String -> Date -> ToDo
newToDo id name content date =
    ToDo id name content Will date


-- ToDoMsg
type Msg
    = Add
    | TurnDone
    | InputName String
    | InputContent String
    | ChangeState State Int
    | ShowContent ShowContent


type ShowContent
    = ShowToDo State
    | ShowAdd


-- ToDoModel
type alias Model =
    { numToDos : Int
    , todos : List ToDo
    , addToDoName : String
    , addToDoContent : String
    , date: Date 
    , showContent : ShowContent
    }


-- init
init : Model
init =
    Model 0 [] "" "" (Date.fromCalendarDate 2020 Jan 1) (ShowToDo Will)


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
                | todos = 
                    model.todos 
                        ++ [ newToDo model.numToDos model.addToDoName 
                                model.addToDoContent model.date ]
                , addToDoName = ""
                , numToDos = model.numToDos + 1
            }

        TurnDone ->
            { model | addToDoName = "" }

        InputName toDoName ->
            { model | addToDoName = toDoName}

        InputContent toDoContent ->
            { model | addToDoContent = toDoContent }

        ChangeState state id ->
            { model
                | todos = updateItem model.todos id state
            }
        ShowContent content ->
            { model | showContent = content}


-- view
view : Model -> Html Msg
view model =
    let 
        cssList = 
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        show = 
            case model.showContent of
                ShowAdd -> viewInput model
                ShowToDo state -> viewItems model state
        buttons = 
            let 
                itemCss = 
                    [ Css.boxSizing Css.borderBox
                    , Css.padding2 (Css.px 10) (Css.px 40) 
                    , Css.backgroundColor (Css.rgb 244 244 244)
                    , Css.border (Css.px 0)
                    , Css.borderRadius (Css.px 5)
                    ]
            in
                div 
                    [ css 
                        [ Css.displayFlex
                        , Css.justifyContent Css.spaceBetween
                        , Css.width (Css.pct 60)
                        , Css.margin2 (Css.px 0) (Css.auto)
                        , Css.paddingTop (Css.px 20)
                        , Css.paddingBottom (Css.px 20)
                        ] 
                    ]
                    [ button 
                        [ onClick (ShowContent (ShowToDo Will)) , css itemCss ]
                        [ text "Will" ]
                    , button 
                        [ onClick (ShowContent (ShowToDo Doing)), css itemCss]
                        [ text "Doing" ]
                    , button 
                        [ onClick (ShowContent (ShowToDo Done)), css itemCss]
                        [ text "Done" ]
                    , button
                        [ onClick (ShowContent ShowAdd), css itemCss ]
                        [ text "Add" ]
                    ]
    in 
        div 
            [css cssList]
            [ buttons
            , show
            ]


viewInput : Model -> Html Msg
viewInput model =
    div 
        [ css 
            [ Css.alignItems Css.center 
            , Css.displayFlex
            , Css.justifyContent Css.center
            , descendants
                [ selector ".form" 
                    [ Css.borderRadius (Css.px 20)
                    , Css.boxSizing Css.borderBox
                    , Css.height (Css.px 700)
                    , Css.padding (Css.px 20)
                    , Css.width (Css.px 400)
                    ]
                , selector ".title"
                    [ Css.fontSize (Css.px 36)
                    , Css.marginTop (Css.px 30)
                    ]
                , selector ".input-container"
                    [ Css.position Css.relative
                    , Css.width (Css.pct 100)
                    ]
                , selector ".ic1"
                    [ Css.marginTop (Css.px 40) ]
                , selector ".ic1"
                    [ Css.marginTop (Css.px 30) ]
                , selector ".input"
                    [ Css.backgroundColor (Css.rgb 244 244 244)
                    , Css.borderRadius (Css.px 12)
                    , Css.border (Css.px 0)
                    , Css.boxSizing Css.borderBox
                    , Css.fontSize (Css.px 18)
                    , Css.padding (Css.px 4)
                    , Css.width (Css.pct 100)
                    ]
                , selector ".submit-button"
                    [ Css.backgroundColor (Css.rgb 0 200 200)
                    , Css.borderRadius (Css.px 12)
                    , Css.border (Css.px 0)
                    , Css.boxSizing (Css.borderBox)
                    , Css.color (Css.rgb 255 255 255)
                    , Css.width (Css.px 100)
                    , Css.height (Css.px 40)
                    , Css.fontSize (Css.px 20)
                    ] 
                , selector ".submit-button-outer"
                    <| if String.length model.addToDoName == 0 then 
                            [Css.display Css.none] 
                        else []
                ]
            ]
        ]
        [ div 
            [ AttrHtml.class "form" ]
            [ div 
                [ AttrHtml.class "title" ]
                [ text "Add To Do task" ]
            , div 
                [ AttrHtml.classList 
                    [ ("input-container", True)
                    , ("ic1", True)
                    ]
                , css [Css.height (Css.px 50)]
                , value model.addToDoName
                , onInput InputName
                ]
                [ input
                    [ AttrHtml.id "firstname"
                    , AttrHtml.class "input"
                    , type_ "text"
                    , placeholder "To Do Name"
                    ]
                    []
                ]

            , div 
                [ AttrHtml.classList 
                    [ ("input-container", True)
                    , ("ic2", True)
                    ]
                , css [Css.height (Css.px 300)]
                ]
                [ textarea
                    [ AttrHtml.id "firstname"
                    , AttrHtml.class "input"
                    , placeholder "To Do Content"
                    , css 
                        [ Css.resize Css.none
                        , Css.flexGrow (Css.num 1)
                        , Css.height (Css.px 250)
                        ]
                    , value model.addToDoContent
                    , onInput InputContent
                    ]
                    []
                ]
            , div [ AttrHtml.class "submit-button-outer" ]
                [ button
                    [ type_ "submit"
                    , AttrHtml.class "submit-button"
                    ]
                    [ text "Add" ]
                ]
            ]
        ]

viewItems : Model -> State -> Html Msg
viewItems model state =
    div [css [ Css.width (Css.pct 100), Css.justifyContent Css.center, Css.displayFlex ]]
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
