module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }



-- MODEL


type Model
  = Failure
  | Loading
  | Success HelloJson


type alias HelloJson = { name : String }


init : () -> (Model, Cmd Msg)
init _ =
  (Loading, getHelloJson)



-- UPDATE


type Msg = GetHelloJson (Result Http.Error HelloJson)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    -- MorePlease ->
    --   (Loading, getRandomQuote)

    GetHelloJson result ->
      case result of
        Ok quote ->
          (Success quote, Cmd.none)

        Err _ ->
          (Failure, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text "Random Quotes" ]
    , viewQuote model
    ]


viewQuote : Model -> Html Msg
viewQuote model =
  case model of
    Failure ->
      div []
        [ text "I could not load a random quote for some reason. "
        -- , button [ onClick MorePlease ] [ text "Try Again!" ]
        ]

    Loading ->
      text "Loading..."

    Success quote ->
      div []
        [ 
          quote.name
        ]



-- HTTP


getHelloJson : Cmd Msg
getHelloJson =
  Http.get
    { url = "localhost:8080"
    , expect = Http.expectJson GetHelloJson HelloJson
    }


quoteDecoder : Decoder HelloJson
quoteDecoder =
  map4 HelloJson
    (field "name" string)
