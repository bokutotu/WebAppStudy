module Calendar exposing (main)

import Browser

import Html exposing ( Html, div, text, button, table, th, tr, td, tbody, thead )
import Html.Events exposing ( onClick )

import Date exposing ( Date, Unit(..) )

import Task exposing ( Task )

import Time exposing (Month(..), Weekday(..))

import DateTimePicker.DateUtils exposing (Day, generateCalendar)
import DateTimePicker.Formatter exposing (fullMonth)


-- MAIN
main: Program () Model Msg
main =
    Browser.document 
    { init = \_ -> init
    , view = view
    , update = update
    , subscriptions = always Sub.none
    }


-- MODEL
type alias Model = 
    { today : Date
    , forcusDate : Maybe Date
    , monthShowDayList : Maybe (List (List Day))
    }


-- MSG
type Msg 
    = UpdateDay Date
    | SelectDate Date
    | NextMonth
    | PrevMonth


-- INIT
init: (Model, Cmd Msg)
init =
    ( Model ( Date.fromCalendarDate 2020 Jan 1 ) Nothing Nothing 
    , Date.today |> Task.perform  UpdateDay
    )


-- VIEW
view: Model -> Browser.Document Msg
view model =
    Browser.Document "Caledar"
    [ 
        div []
        [ div [] [ text ("Today is " ++ (Date.toIsoString model.today ) )] 
        , calendar model
        ]
    ]


dayToString: Day -> String
dayToString day =
    String.fromInt day.day


viewWeek: List Day -> Html Msg
viewWeek week =
    List.map (\day -> td [] [ text (dayToString day) ]) week |> tr [] 


youbiList: List (Html Msg)
youbiList =
    List.singleton ( 
    thead []
    [ tr [] 
      [ th [] [ text "Sun" ] 
      , th [] [ text "Mon" ]
      , th [] [ text "Tue" ]
      , th [] [ text "Wed" ]
      , th [] [ text "THu" ]
      , th [] [ text "Fri" ]
      , th [] [ text "Sat" ]
      ]
    ]
    )


viewButton: String -> Msg -> Html Msg
viewButton massage  msg =
    button [onClick msg] [ text massage ]


calendar: Model -> Html Msg
calendar model =
    let
        calendarDaysList = List.map (\week -> viewWeek week) (Maybe.withDefault [[]] model.monthShowDayList)
        youbiShowList = youbiList
        showList = youbiShowList ++ calendarDaysList
    in
    div [] 
    [ div [] [ text (fullMonth (Date.month (Maybe.withDefault model.today model.forcusDate))) ]
    , div [] [ viewButton "Next Month" NextMonth, viewButton "Previous Month" PrevMonth ]
    , table [] showList
    ]
    -- List.map (\week -> viewWeek week ) month |> table []



-- UPDATE
update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateDay day ->
            let 
                month = Date.month day
                year = Date.year day
            in
                ( 
                    { model | 
                        today = day 
                        , forcusDate = Just day
                        , monthShowDayList = Just ( generateCalendar Sun month year )
                    }
                    , Cmd.none
                )
        SelectDate day ->
            (
                { model | forcusDate = Just day }
                , Cmd.none
            )

        NextMonth ->
            let
                day = Maybe.withDefault ( Date.fromCalendarDate 2020 Jan 1) model.forcusDate
                nextMonthDay = Date.add Months 1 day
                nextMonthYear = Date.year nextMonthDay
                nextMonthMonth = Date.month nextMonthDay
                nextMonthCalendar = generateCalendar Sun nextMonthMonth nextMonthYear
            in
            ( { model | forcusDate = Just nextMonthDay, monthShowDayList = Just nextMonthCalendar }
            , Cmd.none
            )
        PrevMonth -> 
            let
                day = Maybe.withDefault ( Date.fromCalendarDate 2020 Jan 1) model.forcusDate
                prevMonthDay = Date.add Months -1 day
                prevMonthYear = Date.year prevMonthDay
                prevMonthMonth = Date.month prevMonthDay
                prevMonthCalendar = generateCalendar Sun prevMonthMonth prevMonthYear
            in
            ( { model | forcusDate = Just prevMonthDay, monthShowDayList = Just prevMonthCalendar }
            , Cmd.none
            )
