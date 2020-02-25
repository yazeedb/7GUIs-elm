module Crud exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes as A exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


type alias GlobalId =
    Int


type alias Model =
    { globalId : GlobalId
    , filterPrefix : String
    , users : List User
    , focusedUserId : Maybe GlobalId
    }


type alias User =
    { firstName : String
    , lastName : String
    }


type Msg
    = SomeMsg


initialModel : Model
initialModel =
    { globalId = 0
    , filterPrefix = ""
    , users = []
    , focusedUserId = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SomeMsg ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        []


durationMsToSeconds : Int -> Float
durationMsToSeconds ms =
    toFloat ms / 1000
