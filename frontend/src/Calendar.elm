module Calendar exposing (main)

import Browser

import Html exposing ( Html, div, text, button, table, th, tr, td, tbody, thead )
import Html.Events exposing ( onClick )

import Date exposing ( Date, Unit(..) )

import Task exposing ( Task )

import Time exposing (Month(..), Weekday(..))

import DateTimePicker.DateUtils exposing (Day, generateCalendar, MonthType(..))
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
    , showType : ShowType
    }


type ShowType 
    = Month
    | Week


-- MSG
type Msg 
    = UpdateDay Date
    | SelectDate Date
    | NextMonth
    | PrevMonth
    | NextWeek
    | PrevWeek
    | ShowMonth
    | ShowWeek


-- INIT
init: (Model, Cmd Msg)
init =
    ( Model ( Date.fromCalendarDate 2020 Jan 1 ) Nothing Nothing Month
    , Date.today |> Task.perform  UpdateDay
    )


-- VIEW
view: Model -> Browser.Document Msg
view model =
    let 
        showMonth = viewMonth model
        showWeek = viewWeek model
        show = 
            if model.showType == Month then
                showMonth
            else 
                showWeek
        modeChangeString = if model.showType == Month then "Week" else "Month"
        modeChangeMsg = if model.showType == Month then ShowWeek else ShowMonth

    in 
        Browser.Document "Caledar"
        [ 
            div []
            [ div [] [ text ("Today is " ++ (Date.toIsoString model.today ) )] 
            , viewButton modeChangeString modeChangeMsg
            , show
            ]
        ]


dayToString: Day -> String
dayToString day =
    String.fromInt day.day


viewWeekInner: List Day -> Html Msg
viewWeekInner week =
    List.map (\day -> td [] [ text (dayToString day) ]) week |> tr [] 


viewWeek: Model -> Html Msg
viewWeek model =
    let
        forcusDate = Maybe.withDefault model.today model.forcusDate

        forcusDayNum = Date.day forcusDate
        
        forcusDay = Day Current forcusDayNum

        isForcusDay: Day -> Bool
        isForcusDay day =
            day == forcusDay

        containForcusDay week =
            List.any isForcusDay week
        
        getForcusDayWeek: List (List Day) -> List Day
        getForcusDayWeek month =
            List.filter containForcusDay month
                |> List.head 
                |> Maybe.withDefault []

        forcusDayWeek = getForcusDayWeek (Maybe.withDefault [[]] model.monthShowDayList)

        weekShow = List.singleton ( viewWeekInner  forcusDayWeek  )

        showList = (youbiList) ++ weekShow

    in
        div []
        [ div [] [ text (fullMonth (Date.month (Maybe.withDefault model.today model.forcusDate))) ]
        , viewButton "Prev Week" PrevWeek
        , viewButton "Next Week" NextWeek
        , table [] showList
        ]


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


viewMonth: Model -> Html Msg
viewMonth model =
    let
        calendarDaysList = List.map (\week -> viewWeekInner week) (Maybe.withDefault [[]] model.monthShowDayList)
        youbiShowList = youbiList
        showList = youbiShowList ++ calendarDaysList
    in
        div [] 
        [ div [] [ text (fullMonth (Date.month (Maybe.withDefault model.today model.forcusDate))) ]
        , div [] [ viewButton "Next Month" NextMonth, viewButton "Previous Month" PrevMonth]
        , table [] showList
        ]



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

        NextWeek -> 
            let
                day = Maybe.withDefault ( Date.fromCalendarDate 2020 Jan 1) model.forcusDate
                nextWeekDay = Date.add Weeks 1 day
                nextWeekYear = Date.year nextWeekDay
                nextWeekMonth = Date.month nextWeekDay
                nextWeekCalendar = generateCalendar Sun nextWeekMonth nextWeekYear
            in
            ( { model | forcusDate = Just nextWeekDay, monthShowDayList = Just nextWeekCalendar }
            , Cmd.none
            )

        PrevWeek -> 
            let
                day = Maybe.withDefault ( Date.fromCalendarDate 2020 Jan 1) model.forcusDate
                prevWeekDay = Date.add Weeks -1 day
                prevWeekYear = Date.year prevWeekDay
                prevWeekMonth = Date.month prevWeekDay
                prevWeekCalendar = generateCalendar Sun prevWeekMonth prevWeekYear
            in
            ( { model | forcusDate = Just prevWeekDay, monthShowDayList = Just prevWeekCalendar }
            , Cmd.none
            )
        
        ShowMonth -> 
            ( { model | showType = Month }, Cmd.none )

        ShowWeek -> 
            ( { model | showType = Week }, Cmd.none)
