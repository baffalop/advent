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
        , joystick : Joystick
        }


type alias GameInfo a =
    { a
        | score : Int
        , tiles : Game.Tiles
        , frameRate : Float
    }


type Joystick
    = JustDown Game.Joystick
    | HeldDown Game.Joystick
    | Once Game.Joystick
    | Off


type Msg
    = Tick
    | PlayPause
    | ArrowKeyDown Game.Joystick
    | ArrowKeyUp Game.Joystick


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
            advanceGameState model

        PlayPause ->
            case model of
                Playing playState ->
                    PlayingButPaused playState

                PlayingButPaused playState ->
                    Playing playState

                _ ->
                    Playing <| startingState { frameRate = (getGameInfo model).frameRate }

        ArrowKeyDown stick ->
            (\state -> { state | joystick = mapJoystickDown stick state.joystick })
                |> mapPlayState model

        ArrowKeyUp stick ->
            (\state -> { state | joystick = mapJoystickUp stick state.joystick })
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
    , joystick = Off
    , score = 0
    , frameRate = frameRate
    }


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


advanceGameState : Model -> Model
advanceGameState model =
    let
        updatePlaying state =
            let
                nextGameState =
                    Game.play (translateJoystick state.joystick) state.gameState
            in
            case nextGameState of
                Game.Playing _ tiles score ->
                    (\playState ->
                        { playState
                            | gameState = nextGameState
                            , joystick = advanceJoystick state.joystick
                            , tiles = tiles
                            , score = score
                        }
                    )
                        |> mapPlayState model

                Game.GameOver tiles score ->
                    GameOver
                        { tiles = tiles
                        , score = score
                        , frameRate = state.frameRate
                        }

                Game.Error msg ->
                    Error
                        { msg = msg
                        , tiles = state.tiles
                        , score = state.score
                        , frameRate = state.frameRate
                        }
    in
    case model of
        Playing state ->
            updatePlaying state

        PlayingButPaused state ->
            updatePlaying state

        _ ->
            model



-- Keys and joystick


tagKeyDown : Int -> Json.Decoder Msg
tagKeyDown code =
    case Debug.log "KeyDown" code of
        37 ->
            Json.succeed (ArrowKeyDown Game.Left)

        39 ->
            Json.succeed (ArrowKeyDown Game.Right)

        _ ->
            Json.fail "meh"


tagKeyUp : Int -> Json.Decoder Msg
tagKeyUp code =
    case Debug.log "KeyUp" code of
        37 ->
            Json.succeed (ArrowKeyUp Game.Left)

        39 ->
            Json.succeed (ArrowKeyUp Game.Right)

        32 ->
            Json.succeed PlayPause

        _ ->
            Json.fail "w/evs"


translateJoystick : Joystick -> Game.Joystick
translateJoystick state =
    case state of
        JustDown stick ->
            stick

        HeldDown stick ->
            stick

        Once stick ->
            stick

        Off ->
            Game.Neutral


mapJoystickDown : Game.Joystick -> Joystick -> Joystick
mapJoystickDown newStick state =
    case state of
        JustDown _ ->
            JustDown newStick

        HeldDown stick ->
            if newStick == stick then
                state

            else
                JustDown newStick

        Once _ ->
            JustDown newStick

        Off ->
            JustDown newStick


mapJoystickUp : Game.Joystick -> Joystick -> Joystick
mapJoystickUp upStick state =
    case state of
        JustDown stick ->
            if upStick == stick then
                Once stick

            else
                state

        HeldDown stick ->
            if upStick == stick then
                Off

            else
                state

        _ ->
            state


advanceJoystick : Joystick -> Joystick
advanceJoystick state =
    case state of
        Once _ ->
            Off

        JustDown stick ->
            HeldDown stick

        _ ->
            state



-- Type helpers


mapPlayState : Model -> (PlayState -> PlayState) -> Model
mapPlayState model f =
    case model of
        Playing state ->
            Playing (f state)

        PlayingButPaused state ->
            PlayingButPaused (f state)

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
