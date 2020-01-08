module Arcade.Main exposing (..)

import Arcade.Game as Game
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode
import Time
import Utils exposing (flip)


type alias Model =
    { playing : Bool
    , gameState : Game.GameState
    , joystick : Game.Joystick
    , frameRate : Float
    }


type Msg
    = Tick
    | PlayPause
    | ArrowKey Game.Joystick
    | ClearJoystick
    | None


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    { playing = False
    , gameState = Game.init 2
    , joystick = Game.Neutral
    , frameRate = 3
    }
        |> withNoCmd


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        timeSub =
            if model.playing then
                Time.every (1000 / model.frameRate) (always Tick)

            else
                Sub.none
    in
    Sub.batch
        [ timeSub
        , Browser.Events.onKeyDown (Json.Decode.map tagKeyDown Html.Events.keyCode)
        , Browser.Events.onKeyUp (Json.Decode.map tagKeyUp Html.Events.keyCode)
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Tick ->
            { model | gameState = Game.play model.joystick model.gameState }
                |> withNoCmd

        PlayPause ->
            { model | playing = not model.playing }
                |> withNoCmd

        ArrowKey joystick ->
            { model | joystick = joystick }
                |> withNoCmd

        ClearJoystick ->
            { model | joystick = Game.Neutral }
                |> withNoCmd

        None ->
            model |> withNoCmd


view : Model -> Html Msg
view { gameState } =
    pre [ style "margin" "60px" ] [ text (Game.printFromState gameState) ]


withNoCmd : Model -> ( Model, Cmd msg )
withNoCmd =
    flip Tuple.pair Cmd.none


tagKeyDown : Int -> Msg
tagKeyDown code =
    case code of
        37 ->
            ArrowKey Game.Left

        39 ->
            ArrowKey Game.Right

        _ ->
            None


tagKeyUp : Int -> Msg
tagKeyUp code =
    case code of
        37 ->
            ClearJoystick

        39 ->
            ClearJoystick

        32 ->
            PlayPause

        _ ->
            None
