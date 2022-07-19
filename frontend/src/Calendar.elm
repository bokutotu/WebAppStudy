module Calendar exposing (main)

import Browser

import Html exposing ( Html, div, text )

import Date exposing ( Date, Unit(..) )

import Task exposing ( Task )

import Time exposing (Month(..))


-- MAIN

main : Program () Model Msg
main =
  Browser.document 
  { init = always init
  , update = update
  , view = view 
  , subscriptions = always Sub.none
  }


-- MODEL

type alias Model = { today: Date }


-- Msg

type Msg = 
  NextYear
  | NextMonth
  | NextWeek
  | ToDay Date


-- INIT

init: (Model, Cmd Msg)
init = 
  ( { today = Date.fromCalendarDate 2019 Jan 1 }
    , Date.today |> Task.perform ToDay
  )


-- UPDATE

update: Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of 
    NextYear -> 
      ( { model | today = Date.add Years 1 model.today }, Cmd.none )
    NextMonth ->
      ( { model | today = Date.add Months 1 model.today }, Cmd.none )
    NextWeek ->
      ( { model | today = Date.add Days 7 model.today }, Cmd.none )
    ToDay date -> 
      ( { model | today = date }, Cmd.none )


-- VIEW
view: Model -> Browser.Document Msg
view model =
  Browser.Document
  "Calendar"
  [ div []
    [ div [] [ text "ToDay" ]
    , div [] [ text ( Date.toIsoString model.today ) ]

    ]
  ]
