module Arcade.Main exposing (..)

import Arcade.Game as Game
import Browser
import Browser.Events
import Dict
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events
import Json.Decode as Json
import Time
import Utils exposing (flip)


type Model
    = Playing PlayState
    | PlayingButPaused PlayState
    | GameOver (GameInfo {})
    | Error (GameInfo { msg : String })


type alias PlayState =
    GameInfo
        { gameState : Game.GameState
        , joystick : Game.Joystick
        }


type alias GameInfo a =
    { a
        | score : Int
        , tiles : Game.Tiles
        , frameRate : Float
    }


type alias Settings =
    { frameRate : Float
    }


type Msg
    = Tick
    | PlayPause
    | ArrowKey Game.Joystick
    | ClearJoystick Game.Joystick


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : () -> ( Model, Cmd msg )
init _ =
    PlayingButPaused (startingState { frameRate = 3 })
        |> flip Tuple.pair Cmd.none


subscriptions : Model -> Sub Msg
subscriptions state =
    let
        timeSub =
            case state of
                Playing _ ->
                    Time.every (1000 / (getGameInfo state).frameRate) (always Tick)

                _ ->
                    Sub.none
    in
    Sub.batch
        [ timeSub
        , Browser.Events.onKeyDown (Json.andThen tagKeyDown Html.Events.keyCode)
        , Browser.Events.onKeyUp (Json.andThen tagKeyUp Html.Events.keyCode)
        ]


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    ( updateModel msg model, Cmd.none )


updateModel : Msg -> Model -> Model
updateModel msg model =
    case Debug.log "Msg" msg of
        Tick ->
            (\playState -> { playState | gameState = Game.play playState.joystick playState.gameState })
                |> mapPlayState model
                |> checkGameState

        PlayPause ->
            case model of
                Playing playState ->
                    PlayingButPaused playState

                PlayingButPaused playState ->
                    Playing playState

                _ ->
                    Playing <| startingState { frameRate = (getGameInfo model).frameRate }

        ArrowKey joystick ->
            (\playState -> { playState | joystick = joystick })
                |> mapPlayState model

        ClearJoystick joystick ->
            (\playState ->
                if playState.joystick == joystick then
                    { playState | joystick = Game.Neutral }

                else
                    playState
            )
                |> mapPlayState model


view : Model -> Html Msg
view model =
    let
        { tiles, score } =
            getGameInfo model

        gameBoard =
            Game.print tiles
    in
    div []
        [ pre [ style "margin" "60px" ] [ text gameBoard ]
        , pre [ style "margin-left" "60px", style "margin-top" "30px" ]
            [ text (printState model) ]
        ]



-- Game logic


startingState : { frameRate : Float } -> PlayState
startingState { frameRate } =
    let
        gameState =
            Game.init 2
    in
    { gameState = gameState
    , tiles = getTiles gameState
    , joystick = Game.Neutral
    , score = 0
    , frameRate = frameRate
    }


mapPlayState : Model -> (PlayState -> PlayState) -> Model
mapPlayState model f =
    case model of
        Playing state ->
            Playing (f state)

        PlayingButPaused state ->
            PlayingButPaused (f state)

        _ ->
            model


checkGameState : Model -> Model
checkGameState model =
    let
        mapPlaying { gameState, tiles, score, frameRate } =
            case gameState of
                Game.Playing _ _ _ ->
                    model

                Game.GameOver gameTiles gameScore ->
                    GameOver
                        { tiles = gameTiles
                        , score = gameScore
                        , frameRate = frameRate
                        }

                Game.Error msg ->
                    Error
                        { msg = msg
                        , tiles = tiles
                        , score = score
                        , frameRate = frameRate
                        }
    in
    case model of
        Playing playState ->
            mapPlaying playState

        PlayingButPaused playState ->
            mapPlaying playState

        _ ->
            model


getTiles : Game.GameState -> Game.Tiles
getTiles state =
    case state of
        Game.Playing _ tiles _ ->
            tiles

        Game.GameOver tiles _ ->
            tiles

        Game.Error _ ->
            Dict.empty


getGameInfo : Model -> GameInfo {}
getGameInfo state =
    case state of
        Playing { tiles, score, frameRate } ->
            { tiles = tiles, score = score, frameRate = frameRate }

        PlayingButPaused { tiles, score, frameRate } ->
            { tiles = tiles, score = score, frameRate = frameRate }

        GameOver { tiles, score, frameRate } ->
            { tiles = tiles, score = score, frameRate = frameRate }

        Error { tiles, score, frameRate } ->
            { tiles = tiles, score = score, frameRate = frameRate }


tagKeyDown : Int -> Json.Decoder Msg
tagKeyDown code =
    case Debug.log "KeyDown" code of
        37 ->
            Json.succeed (ArrowKey Game.Left)

        39 ->
            Json.succeed (ArrowKey Game.Right)

        _ ->
            Json.fail "meh"


tagKeyUp : Int -> Json.Decoder Msg
tagKeyUp code =
    case Debug.log "KeyUp" code of
        37 ->
            Json.succeed (ClearJoystick Game.Left)

        39 ->
            Json.succeed (ClearJoystick Game.Right)

        32 ->
            Json.succeed PlayPause

        _ ->
            Json.fail "w/evs"


printState : Model -> String
printState model =
    let
        { score } =
            getGameInfo model

        scoreMsg =
            "Score : " ++ String.fromInt score ++ "     "
    in
    case model of
        Error { msg } ->
            scoreMsg ++ "ERROR: " ++ msg

        GameOver _ ->
            scoreMsg ++ "GAME OVER"

        _ ->
            scoreMsg
