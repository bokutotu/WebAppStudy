module Main exposing (..)

-- import Browser
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, map4, field, int, string)

-- -- MAIN


-- main =
--   Browser.element {
--     init = init,
--     view = view,
--     update = update,
--     subscription = subscription
--   }

-- type alias Model = Int

-- init: Model -> (Model, Cmd)
-- init Model = 
--   (0, Cmd.none)

-- type alias Msg = String

-- view: Msg -> Html Msg

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--


import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model = String


init : Model
init =
  "Hello World"



-- UPDATE


type alias Msg = String


update : Msg -> Model -> Model
update msg model =
  "Hello World"


   -- VIEW


view : Model -> Html Msg
view model =
  div []
    [
      text model 
    ]
