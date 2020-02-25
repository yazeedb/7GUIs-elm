module Timer exposing (..)

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


type alias Model =
    { elapsedTimeMs : Float
    , durationMs : Int
    }


type Msg
    = UpdateElapsedTime Float
    | UpdateDuration String
    | ResetElapsedTime
    | Tick Time.Posix


initialModel : Model
initialModel =
    { elapsedTimeMs = 0
    , durationMs = 2000
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateElapsedTime newTime ->
            ( { model | elapsedTimeMs = newTime }, Cmd.none )

        UpdateDuration value ->
            let
                newDurationMs =
                    Maybe.withDefault model.durationMs (String.toInt value)
            in
            ( { model | durationMs = newDurationMs }, Cmd.none )

        ResetElapsedTime ->
            ( { model | elapsedTimeMs = initialModel.elapsedTimeMs }, Cmd.none )

        Tick _ ->
            if model.elapsedTimeMs >= toFloat model.durationMs then
                ( model, Cmd.none )

            else
                ( { model | elapsedTimeMs = model.elapsedTimeMs + 100 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.elapsedTimeMs >= toFloat model.durationMs then
        Sub.none

    else
        Time.every 100 Tick


view : Model -> Html Msg
view model =
    div []
        [ label [] [ text "Elapsed time:" ]
        , div []
            [ text
                (String.fromFloat (model.elapsedTimeMs / 1000)
                    ++ "s / "
                    ++ String.fromFloat (durationMsToSeconds model.durationMs)
                    ++ "s"
                )
            ]
        , input
            [ type_ "range"
            , value (String.fromInt model.durationMs)
            , onInput UpdateDuration
            , A.min "0"
            , A.max "30000"
            , A.step "1000"
            ]
            []
        , button [ onClick ResetElapsedTime ] [ text "Reset" ]
        ]


durationMsToSeconds : Int -> Float
durationMsToSeconds ms =
    toFloat ms / 1000
