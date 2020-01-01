module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Counter
import Html exposing (a, div, nav, text)
import Html.Attributes exposing (href)
import TempConverter
import Url
import Url.Parser exposing (Parser, map, oneOf, s)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model
        key
        url
        (urlToPage url)
        Counter.initialModel
        TempConverter.initialModel
    , Cmd.none
    )


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentPage : Page
    , counterModel : Counter.Model
    , tempConverterModel : TempConverter.Model
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CounterMsg Counter.Msg
    | TempConverterMsg TempConverter.Msg


type Page
    = Home
    | Counter
    | TempConverter
    | PageNotFound


urlParser : Parser (Page -> a) a
urlParser =
    oneOf
        [ map Home Url.Parser.top
        , map Counter (s "counter")
        , map TempConverter (s "temp-converter")
        ]


urlToPage : Url.Url -> Page
urlToPage url =
    url
        |> Url.Parser.parse urlParser
        |> Maybe.withDefault PageNotFound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , currentPage = urlToPage url
              }
            , Cmd.none
            )

        CounterMsg counterMsg ->
            Counter.update counterMsg model.counterModel
                |> Tuple.mapFirst (\cModel -> { model | counterModel = cModel })
                |> Tuple.mapSecond (Cmd.map CounterMsg)

        TempConverterMsg tempConverterMsg ->
            TempConverter.update tempConverterMsg model.tempConverterModel
                |> Tuple.mapFirst (\tModel -> { model | tempConverterModel = tModel })
                |> Tuple.mapSecond (Cmd.map TempConverterMsg)


view : Model -> Browser.Document Msg
view model =
    { title = "Hello!"
    , body =
        [ nav []
            [ a [ href "/" ] [ text "Home" ]
            , a [ href "/counter" ] [ text "Counter" ]
            , a [ href "/temp-converter" ] [ text "Temperature Converter" ]
            ]
        , case model.currentPage of
            Home ->
                div [] [ text "Home page!" ]

            Counter ->
                Counter.view model.counterModel
                    |> Html.map CounterMsg

            TempConverter ->
                TempConverter.view model.tempConverterModel
                    |> Html.map TempConverterMsg

            PageNotFound ->
                div [] [ text "Page not found!" ]
        ]
    }
