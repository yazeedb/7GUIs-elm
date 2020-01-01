module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Counter
import Html exposing (a, div, nav, text)
import Html.Attributes exposing (href)
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
    ( Model key url (urlToPage url), Cmd.none )


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , currentPage : Page
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


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


view : Model -> Browser.Document Msg
view model =
    { title = "Hello!"
    , body =
        [ nav []
            [ a [ href "/" ] [ text "Home" ]
            , a [ href "/counter" ] [ text "Counter" ]
            , a [ href "/temp-converter" ] [ text "Temperature Converter" ]
            ]
        , pageToView model.currentPage
        ]
    }


pageToView : Page -> Html.Html Msg
pageToView page =
    case page of
        Home ->
            div [] [ text "Home page!" ]

        Counter ->
            Counter.main

        TempConverter ->
            div [] [ text "Temp converter" ]

        PageNotFound ->
            div [] [ text "Page not found!" ]
