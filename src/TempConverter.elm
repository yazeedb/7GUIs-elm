module TempConverter exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onInput)


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


type alias Model =
    { celcius : String
    , fahrenheit : String
    }


initialModel : Model
initialModel =
    { celcius = ""
    , fahrenheit = ""
    }


type Msg
    = ChangeCelcius String
    | ChangeFahrenheit String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeCelcius value ->
            let
                trimmedValue =
                    String.trim value
            in
            case String.toFloat trimmedValue of
                Just newC ->
                    let
                        newF =
                            celciusToFahrenheit newC
                    in
                    ( { model
                        | celcius = String.fromFloat newC
                        , fahrenheit = String.fromFloat newF
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | celcius = trimmedValue
                        , fahrenheit = initialModel.fahrenheit
                      }
                    , Cmd.none
                    )

        ChangeFahrenheit value ->
            let
                trimmedValue =
                    String.trim value
            in
            case String.toFloat trimmedValue of
                Just newF ->
                    let
                        newC =
                            fahrenheitToCelcius newF
                    in
                    ( { model
                        | celcius = String.fromFloat newC
                        , fahrenheit = String.fromFloat newF
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model
                        | fahrenheit = trimmedValue
                        , celcius = initialModel.celcius
                      }
                    , Cmd.none
                    )


updateFormValues : String -> Model -> ( Model, Cmd Msg )
updateFormValues value model =
    let
        trimmedValue =
            String.trim value
    in
    case String.toFloat trimmedValue of
        Just newF ->
            let
                newC =
                    fahrenheitToCelcius newF
            in
            ( { model
                | celcius = String.fromFloat newC
                , fahrenheit = String.fromFloat newF
              }
            , Cmd.none
            )

        Nothing ->
            ( { model
                | fahrenheit = trimmedValue
                , celcius = initialModel.celcius
              }
            , Cmd.none
            )


celciusToFahrenheit : Float -> Float
celciusToFahrenheit c =
    c * (9 / 5) + 32


fahrenheitToCelcius : Float -> Float
fahrenheitToCelcius f =
    (f - 32) * (5 / 9)


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ input [ onInput ChangeCelcius, value model.celcius, type_ "number" ] []
            , label [] [ text "Celius = " ]
            , input [ onInput ChangeFahrenheit, value model.fahrenheit, type_ "number" ] []
            , label [] [ text "Fahrenheit" ]
            ]
        ]
