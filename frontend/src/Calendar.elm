module Calendar exposing (Msg, view, Model, update, init)


import Css
import Css.Global exposing (descendants, selector)
import Html.Styled exposing (div, header, Html, thead, tr, th, text, table, h1, td, button, input, label)
import Html.Styled.Events exposing (onClick)
import Html.Styled.Attributes as AttrHtml
import Html.Styled.Attributes exposing (css)

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
                Html.Styled.map ToDoMsg showDay
        modeChangeString = if model.showType == MonthCalendar then "Week" else "Month"
        modeChangeMsg = if model.showType == MonthCalendar then ShowWeek else ShowMonth
    in 
        div 
            [ css 
              [ Css.margin2 Css.zero Css.auto
              , Css.width (Css.pct 60)
              , Css.flexDirection Css.column
              ]
            ]
            [ 
                viewHeader model
                , show
            ]


viewHeader: Model -> Html Msg
viewHeader model =
    let 
        rightArrow = if model.showType == MonthCalendar then NextMonth else NextWeek
        leftArrow = if model.showType == MonthCalendar then PrevMonth else PrevWeek

        selectCss = [ Css.border3 (Css.px 1) Css.solid (Css.hex "efefef")
                    , Css.borderBottomColor Css.transparent
                    , Css.borderTopLeftRadius <| Css.px 2
                    , Css.borderTopRightRadius <| Css.px 2
                    , Css.width (Css.px 70)
                    ]
        notSelectCss = [ Css.borderBottom3 (Css.px 1) Css.solid (Css.hex "efefef") 
                       , Css.width (Css.px 70)
                       ]
        css_: List Css.Style
        css_ =
            [ descendants
                [ selector ".tabs"
                    [ Css.marginTop (Css.px 50)
                    -- , Css.paddingBottom (Css.px 40)
                    -- , Css.backgroundColor (Css.rgb 255 255 255)
                    -- , Css.boxShadow4 (Css.px 0) (Css.px 0) (Css.px 10) (Css.rgba 0 0 0 0.2)
                    , Css.width (Css.pct 100)
                    , Css.margin2 (Css.px 0) (Css.auto)
                    ]
                , selector ".tab_item"
                    [ Css.height (Css.px 40)
                    -- , Css.border3 (Css.px 1) Css.solid (Css.rgba 0 0 0 0.1 )
                    -- , Css.backgroundColor (Css.rgb 217 217 217)
                    , Css.lineHeight (Css.px 50)
                    , Css.fontSize (Css.px 20)
                    , Css.textAlign Css.center
                    , Css.color (Css.rgb 86 86 86)
                    , Css.display Css.block
                    , Css.float Css.left
                    , Css.textAlign Css.center
                    , Css.fontWeight Css.bold
                    , Css.hover [ Css.opacity (Css.num 0.75) ]
                    ]
                ]
            ]
        yearString: String
        yearString =
            Maybe.withDefault (Date.fromCalendarDate 2020 Jan 1) model.forcusDate
                |> Date.year
                |> String.fromInt
        monthString: String
        monthString =
            Maybe.withDefault (Date.fromCalendarDate 2020 Jan 1) model.forcusDate
                |> Date.month
                |> fullMonth
        headerDateString: String
        headerDateString =
            yearString ++ " " ++ monthString

    in
        div [css css_] 
            [ div 
                [ AttrHtml.class "tabs" ]
                [ div [] 
                    [ label 
                        [ AttrHtml.class "tab_item", css [Css.width (Css.pct 40)] ]
                        [ text headerDateString ] 
                    ]

                , div
                    [ onClick ShowMonth ] 
                    [ label 
                        [ AttrHtml.class "tab_item", css [Css.width (Css.pct 15)]]
                        [ text "Month" ] 
                    ]

                , div
                    [ onClick ShowWeek ] 
                    [ label 
                        [ AttrHtml.class "tab_item", css [Css.width (Css.pct 15)]] 
                        [ text "Week" ]
                    ]

                , div
                    [ onClick ( if model.showType == MonthCalendar then PrevMonth else PrevWeek ) ] 
                    [ label 
                        [ AttrHtml.class "tab_item", css [Css.width (Css.pct 2)]]
                        [ text "<" ] 
                    ]

                , div
                    [ onClick ( if model.showType == MonthCalendar then NextMonth else NextWeek ) ] 
                    [ label 
                        [ AttrHtml.class "tab_item", css [Css.width (Css.pct 2)]]
                        [ text ">" ] 
                    ]
                ]
            ]
        
dayToString: Day -> String
dayToString day =
    String.fromInt day.day


youbiList: List (Html Msg)
youbiList =
    let 
        cssList =
            [ descendants
                [ selector ".youbi"
                    [ Css.width (Css.pct 100) ]
                , selector ".youbi_item"
                    [ Css.width (Css.pct (100/7))
                    , Css.lineHeight (Css.px 30)
                    , Css.fontSize (Css.px 16)
                    , Css.color (Css.rgb 86 86 86  )
                    , Css.fontWeight Css.bold
                    ]
                ]
            ]
    in 
        List.singleton ( 
        thead [ css cssList ]
        [ tr [ AttrHtml.class "youbi" ]
          [ th [AttrHtml.class "youbi_item"] [ text "Sun" ] 
          , th [AttrHtml.class "youbi_item"] [ text "Mon" ]
          , th [AttrHtml.class "youbi_item"] [ text "Tue" ]
          , th [AttrHtml.class "youbi_item"] [ text "Wed" ]
          , th [AttrHtml.class "youbi_item"] [ text "Thu" ]
          , th [AttrHtml.class "youbi_item"] [ text "Fri" ]
          , th [AttrHtml.class "youbi_item"] [ text "Sat" ]
          ]
        ]
        )


viewButton: String -> Msg -> Html Msg
viewButton massage  msg =
    div [onClick msg, css []] [ Html.Styled.text massage ]


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
    List.map (\day -> td [ AttrHtml.class "day_item", css [Css.verticalAlign Css.top] ]
        [ div 
            [ css [ Css.height (Css.px 10) ] ] 
            [viewButton (dayToString day) (ShowDay (dayToDate day))] 
        ]) week |> Html.Styled.tr []



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

        weekShow = List.singleton ( viewWeekInner (Date.year forcusDate) (Date.month forcusDate)  forcusDayWeek )

        showList = (youbiList) ++ weekShow

        cssList = 
            [ descendants
                [ selector ".day_item"
                    [ Css.height (Css.pct 100)
                    , Css.textAlign Css.center
                    , Css.hover [Css.opacity (Css.num 0.75)]
                    , Css.fontSize (Css.px 20)
                    , Css.cursor Css.pointer
                    , Css.color (Css.rgb 86 86 86)
                    , Css.fontWeight Css.bold
                    ]
                ]
            ]

    in
        div [ css cssList ]
        [ table [ css [Css.tableLayout Css.fixed, Css.width (Css.pct 100)] ] showList ]



viewMonth: Model -> Html Msg
viewMonth model =
    let 
        forcusDate = Maybe.withDefault model.today model.forcusDate
        calendarDaysList = List.map 
            (\week -> viewWeekInner (Date.year forcusDate) 
            (Date.month forcusDate) week) (Maybe.withDefault [[]] model.monthShowDayList)
        youbiShowList = youbiList
        showList = youbiShowList ++ calendarDaysList
        cssList = 
            [ descendants
                [ selector ".day_item" 
                    [ Css.height (Css.px 100)
                    , Css.textAlign Css.center
                    , Css.hover [Css.opacity (Css.num 0.75)]
                    , Css.fontSize (Css.px 20)
                    , Css.cursor Css.pointer
                    , Css.color (Css.rgb 86 86 86  )
                    , Css.fontWeight Css.bold
                    ]
                ]
            ]
    in
        div [css cssList] 
        [ 
        div [ ] 
            [ table [ css [Css.tableLayout Css.fixed, Css.width (Css.pct 100)] ] showList ]
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
                
