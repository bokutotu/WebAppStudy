module Calendar exposing (Msg, view, Model, update, init)

import Html exposing ( Html, div, text, button, table, th, tr, td, thead )
import Html.Events exposing ( onClick )

import Date exposing ( Date, Unit(..) )

import Task exposing ( .. )

import Time exposing (Month(..), Weekday(..))

import DateTimePicker.DateUtils exposing (Day, generateCalendar, MonthType(..))
import DateTimePicker.Formatter exposing (fullMonth)

import ToDo exposing (..)



-- MODEL
type alias Model = 
    { today : Date
    , forcusDate : Maybe Date
    , monthShowDayList : Maybe (List (List Day))
    , showType : ShowType
    , todo : ToDo.Model
    }


type ShowType 
    = MonthCalendar
    | WeekCalendar
    | DayCalendar


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
    | ShowDay Date
    | ToDoMsg ToDo.Msg


-- INIT
init: (Model, Cmd Msg)
init =
    ( Model ( Date.fromCalendarDate 2020 Jan 1 ) Nothing Nothing MonthCalendar (ToDo.init)
    , Date.today |> Task.perform  UpdateDay
    )


-- VIEW
view: Model -> Html Msg
view model =
    let 
        showMonth = viewMonth model
        showWeek = viewWeek model
        showDay = ToDo.view model.todo
        show = 
            if model.showType == MonthCalendar then
                showMonth
            else if model.showType == WeekCalendar then
                showWeek
            else
                Html.map ToDoMsg showDay
        modeChangeString = if model.showType == MonthCalendar then "Week" else "Month"
        modeChangeMsg = if model.showType == MonthCalendar then ShowWeek else ShowMonth
    in 
        div []
        [ div [] [ text ("Today is " ++ (Date.toIsoString model.today ) )] 
        , viewButton modeChangeString modeChangeMsg
        , show
        ]


dayToString: Day -> String
dayToString day =
    String.fromInt day.day


viewWeekInner: Int -> Month -> List Day -> Html Msg
viewWeekInner year month week =
    let
        msgYear: Day -> Int
        msgYear day =
            if month == Jan && day.monthType == Previous then
                year - 1
            else if month == Dec && day.monthType == Next then
                year + 1
            else 
                year

        msgMonth: Day -> Month
        msgMonth day =
            Date.monthToNumber month
                |> (\x -> x +  (if day.monthType == Previous then -1 else if day.monthType == Next then 1 else 0) )
                |> (\monthNum -> if monthNum == -1 then 12 else if monthNum == 13 then 1 else monthNum)
                |> Date.numberToMonth
        
        dayToDate: Day -> Date
        dayToDate day =
            Date.fromCalendarDate (msgYear day) (msgMonth day) day.day
    in 
        List.map (\day -> td [] [ viewButton (dayToString day) (ShowDay (dayToDate day)) ]) week |> tr [] 


viewWeek: Model -> Html Msg
viewWeek model =
    let
        _ = Debug.log "Date" (List.map (\todo -> Date.toIsoString todo.date) model.todo.todos)
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

        weekShow = List.singleton ( viewWeekInner (Date.year forcusDate) (Date.month forcusDate)  forcusDayWeek )

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
        forcusDate = Maybe.withDefault model.today model.forcusDate
        calendarDaysList = List.map (\week -> viewWeekInner (Date.year forcusDate) (Date.month forcusDate) week) (Maybe.withDefault [[]] model.monthShowDayList)
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
    let
        initDate = Date.fromCalendarDate 2020 Jan 1

        unwrapWithDefaultDate: Maybe Date -> Date
        unwrapWithDefaultDate date =
            Maybe.withDefault initDate date
        
        calendarUpdate: Unit -> Int -> Maybe Date -> Model
        calendarUpdate calUnit calNum date =
            let 
                day = unwrapWithDefaultDate date
                calDay = Date.add calUnit calNum day
                calYear = Date.year calDay
                calMonth = Date.month calDay
                calendar = generateCalendar Sun calMonth calYear
            in
                { model | forcusDate = Just calDay, monthShowDayList = Just calendar }

    in
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
                ( (calendarUpdate Months 1 model.forcusDate)
                , Cmd.none
                )

            PrevMonth -> 
                ( (calendarUpdate Months -1 model.forcusDate)
                , Cmd.none
                )

            NextWeek -> 
                ( (calendarUpdate Weeks 1 model.forcusDate)
                , Cmd.none
                )

            PrevWeek ->
                ( (calendarUpdate Weeks -1 model.forcusDate)
                , Cmd.none
                )

            ShowMonth -> 
                ( { model | showType = MonthCalendar }, Cmd.none )

            ShowWeek -> 
                ( { model | showType = WeekCalendar }, Cmd.none)

            ShowDay date -> 
                let 
                    todoModel = ToDo.updateDate date model.todo
                in
                    ( { model | showType = DayCalendar, forcusDate = Just date, todo = todoModel }, Cmd.none )

            ToDoMsg todoMsg ->
                ( { model | todo = ToDo.update todoMsg model.todo }, Cmd.none )
                
