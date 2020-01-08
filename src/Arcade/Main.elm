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


type alias Model =
    ( State, Settings )


type State
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
    ( PlayingButPaused startingState, { frameRate = 3 } )
        |> flip Tuple.pair Cmd.none


subscriptions : Model -> Sub Msg
subscriptions ( state, settings ) =
    let
        timeSub =
            case state of
                Playing _ ->
                    Time.every (1000 / settings.frameRate) (always Tick)

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
updateModel msg ( state, settings ) =
    let
        withSettings =
            flip Tuple.pair settings
    in
    case Debug.log "Msg" msg of
        Tick ->
            (\playState -> { playState | gameState = Game.play playState.joystick playState.gameState })
                |> mapPlayState state
                |> checkGameState
                |> withSettings

        PlayPause ->
            case state of
                Playing playState ->
                    PlayingButPaused playState |> withSettings

                PlayingButPaused playState ->
                    Playing playState |> withSettings

                _ ->
                    Playing startingState |> withSettings

        ArrowKey joystick ->
            (\playState -> { playState | joystick = joystick })
                |> mapPlayState state
                |> withSettings

        ClearJoystick joystick ->
            (\playState ->
                if playState.joystick == joystick then
                    { playState | joystick = Game.Neutral }

                else
                    playState
            )
                |> mapPlayState state
                |> withSettings


view : Model -> Html Msg
view ( state, _ ) =
    let
        { tiles, score } =
            getGameInfo state

        gameBoard =
            Game.print tiles
    in
    div []
        [ pre [ style "margin" "60px" ] [ text gameBoard ]
        , pre [ style "margin-left" "60px", style "margin-top" "30px" ]
            [ text (printState state) ]
        ]



-- Game logic


startingState : PlayState
startingState =
    let
        gameState =
            Game.init 2
    in
    { gameState = gameState
    , tiles = getTiles gameState
    , joystick = Game.Neutral
    , score = 0
    }


mapPlayState : State -> (PlayState -> PlayState) -> State
mapPlayState state f =
    case state of
        Playing playState ->
            Playing (f playState)

        PlayingButPaused playState ->
            PlayingButPaused (f playState)

        _ ->
            state


checkGameState : State -> State
checkGameState state =
    let
        mapPlaying { gameState, tiles, score } =
            case gameState of
                Game.Playing _ _ _ ->
                    state

                Game.GameOver gameTiles gameScore ->
                    GameOver { tiles = gameTiles, score = gameScore }

                Game.Error msg ->
                    Error
                        { msg = msg
                        , tiles = tiles
                        , score = score
                        }
    in
    case state of
        Playing playState ->
            mapPlaying playState

        PlayingButPaused playState ->
            mapPlaying playState

        _ ->
            state


getTiles : Game.GameState -> Game.Tiles
getTiles state =
    case state of
        Game.Playing _ tiles _ ->
            tiles

        Game.GameOver tiles _ ->
            tiles

        Game.Error _ ->
            Dict.empty


getGameInfo : State -> GameInfo {}
getGameInfo state =
    case state of
        Playing { tiles, score } ->
            { tiles = tiles, score = score }

        PlayingButPaused { tiles, score } ->
            { tiles = tiles, score = score }

        GameOver { tiles, score } ->
            { tiles = tiles, score = score }

        Error { tiles, score } ->
            { tiles = tiles, score = score }


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


printState : State -> String
printState state =
    let
        { score } =
            getGameInfo state

        scoreMsg =
            "Score : " ++ String.fromInt score ++ "     "
    in
    case state of
        Error { msg } ->
            scoreMsg ++ "ERROR: " ++ msg

        GameOver _ ->
            scoreMsg ++ "GAME OVER"

        _ ->
            scoreMsg
