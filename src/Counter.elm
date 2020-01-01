module Counter exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    Int


type Msg
    = Increment


initialModel : Model
initialModel =
    0


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ pre [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "Count" ]
        ]
