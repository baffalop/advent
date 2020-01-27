module Oxygen.Game exposing (..)

import Browser
import Browser.Events
import Element exposing (Element, el, rgb255, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Json
import Oxygen.Robot as R



-- TYPES


type alias Model =
    Result String R.Robot


type Msg
    = Reset
    | KeyPress R.Direction



-- MAIN, UPDATE...


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    ( Ok R.init, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case msg of
        Reset ->
            Ok R.init

        KeyPress direction ->
            Result.andThen (R.move direction) model


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown (Json.andThen tagKeyDown Html.Events.keyCode)
        , Browser.Events.onKeyUp (Json.andThen tagKeyUp Html.Events.keyCode)
        ]



-- VIEW


view : Model -> Html msg
view model =
    Element.layout
        [ Background.color backgroundColor
        , Font.color (rgb255 204 204 204)
        , Font.family
            [ Font.external
                { name = "Space"
                , url = "https://fonts.googleapis.com/css?family=Space+Mono&display=swap"
                }
            , Font.monospace
            ]
        ]
    <|
        panel (viewText model)


viewText : Model -> Element msg
viewText model =
    case model of
        Err msg ->
            text msg

        Ok ( _, map ) ->
            R.print map |> pre


panel : Element msg -> Element msg
panel content =
    el
        [ Border.color (rgb255 220 213 127)
        , Border.solid
        , Border.width 1
        , Element.padding 5
        , Element.centerX
        , Element.centerY
        ]
        content


backgroundColor : Element.Color
backgroundColor =
    rgb255 15 15 35


pre : String -> Element msg
pre =
    Html.text
        >> List.singleton
        >> Html.pre [ Html.Attributes.style "margin" "0" ]
        >> Element.html


br : Element msg
br =
    Element.html <| Html.br [] []



-- keys


tagKeyDown : Int -> Json.Decoder Msg
tagKeyDown code =
    case Debug.log "Keydown" code of
        -- left arrow
        37 ->
            Json.succeed (KeyPress R.West)

        -- up arrow
        38 ->
            Json.succeed (KeyPress R.North)

        -- right arrow
        39 ->
            Json.succeed (KeyPress R.East)

        -- down arrow
        40 ->
            Json.succeed (KeyPress R.South)

        _ ->
            Json.fail "meh"


tagKeyUp : Int -> Json.Decoder Msg
tagKeyUp code =
    case code of
        -- 'r'
        82 ->
            Json.succeed Reset

        _ ->
            Json.fail "w/evs"
