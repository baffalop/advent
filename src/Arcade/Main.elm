module Arcade.Main exposing (..)

import Arcade.Game as Game
import Browser
import Browser.Events
import Dict
import Element exposing (Element, el, rgb255, text)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Html exposing (Html)
import Html.Events
import Json.Decode as Json
import Time
import Utils exposing (flip)


type Model
    = Playing PlayState
    | PlayingButPaused PlayState
    | GameOver (GameInfo { finalState : Game.GameState })
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
    = JustPressed Game.Joystick
    | HeldDown Game.Joystick
    | Once Game.Joystick
    | Off


type Msg
    = Tick
    | PlayPause
    | Reset
    | ArrowKeyDown Game.Joystick
    | ArrowKeyUp Game.Joystick
    | IncreaseFrameRate
    | DecreaseFrameRate


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
    case msg of
        Tick ->
            advanceGameState model

        PlayPause ->
            case model of
                Playing playState ->
                    PlayingButPaused playState

                PlayingButPaused playState ->
                    Playing playState

                GameOver finalState ->
                    cheat finalState

                Error { frameRate } ->
                    Playing <| startingState { frameRate = frameRate }

        Reset ->
            let
                { frameRate } =
                    getGameInfo model
            in
            Playing <| startingState { frameRate = (getGameInfo model).frameRate }

        ArrowKeyDown stick ->
            (\state -> { state | joystick = mapJoystickDown stick state.joystick })
                |> mapPlayState model

        ArrowKeyUp stick ->
            (\state -> { state | joystick = mapJoystickUp stick state.joystick })
                |> mapPlayState model

        IncreaseFrameRate ->
            mapGameInfo model (\state -> { state | frameRate = state.frameRate * 1.5 })

        DecreaseFrameRate ->
            mapGameInfo model (\state -> { state | frameRate = state.frameRate / 1.5 })



-- VIEW


view : Model -> Html msg
view model =
    let
        { tiles } =
            getGameInfo model

        gameBoard =
            Game.print tiles
    in
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
        el [] <|
            Element.column
                [ Element.centerX, Element.centerY, Element.spacing 30 ]
                [ Element.row [ Element.spacing 30 ] [ gamePanel gameBoard, instructionsPanel ]
                , scorePanel model (getTextWidth gameBoard)
                ]


gamePanel : String -> Element msg
gamePanel =
    panel Element.none
        << Element.html
        << Html.pre []
        << List.singleton
        << Html.text


scorePanel : Model -> Int -> Element msg
scorePanel model width =
    printState model width
        |> Element.text
        |> panel Element.none


instructionsPanel : Element msg
instructionsPanel =
    panel Element.none <|
        Element.column
            [ Element.width (Element.px 120) ]
            [ text "ARROW KEYS - Move"
            , text "<SPACE> - Play/pause"
            , text "R - Reset"
            , text "A/Z - Increase/decrease framerate"
            ]


panel : Element msg -> Element msg -> Element msg
panel modal content =
    el
        [ Border.color (rgb255 220 213 127)
        , Border.solid
        , Border.width 2
        , Element.padding 4
        , Element.inFront modal
        , Element.alignTop
        ]
        content


innerPanel : Element msg -> Element msg
innerPanel content =
    el
        [ Element.centerX
        , Element.centerY
        , Element.padding 2
        , Background.color backgroundColor
        ]
        (panel content Element.none)


backgroundColor : Element.Color
backgroundColor =
    rgb255 15 15 35



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

                Game.GameOver _ tiles score ->
                    GameOver
                        { tiles = tiles
                        , score = score
                        , frameRate = state.frameRate
                        , finalState = nextGameState
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


cheat : GameInfo { finalState : Game.GameState } -> Model
cheat { finalState, tiles, score, frameRate } =
    let
        restartedGame =
            Game.cheat finalState
    in
    case restartedGame of
        Game.Playing _ newTiles newScore ->
            Playing
                { gameState = restartedGame
                , joystick = Off
                , tiles = newTiles
                , score = newScore
                , frameRate = frameRate
                }

        Game.GameOver _ _ _ ->
            Error
                { msg = "Attempted cheating resulted in GameOver state"
                , tiles = tiles
                , score = score
                , frameRate = frameRate
                }

        Game.Error msg ->
            Error
                { msg = "Attempted cheating resulted in Error state: " ++ msg
                , tiles = tiles
                , score = score
                , frameRate = frameRate
                }


mapGameInfo : Model -> (GameInfo {} -> GameInfo {}) -> Model
mapGameInfo model f =
    let
        { tiles, score, frameRate } =
            f (getGameInfo model)

        map state =
            { state | tiles = tiles, score = score, frameRate = frameRate }
    in
    case model of
        GameOver state ->
            GameOver (map state)

        Error state ->
            Error (map state)

        _ ->
            mapPlayState model map



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

        82 ->
            Json.succeed Reset

        65 ->
            Json.succeed IncreaseFrameRate

        90 ->
            Json.succeed DecreaseFrameRate

        _ ->
            Json.fail "w/evs"


translateJoystick : Joystick -> Game.Joystick
translateJoystick state =
    case state of
        JustPressed stick ->
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
        JustPressed _ ->
            JustPressed newStick

        HeldDown stick ->
            if newStick == stick then
                state

            else
                JustPressed newStick

        Once _ ->
            JustPressed newStick

        Off ->
            JustPressed newStick


mapJoystickUp : Game.Joystick -> Joystick -> Joystick
mapJoystickUp upStick state =
    case state of
        JustPressed stick ->
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

        JustPressed stick ->
            HeldDown stick

        _ ->
            state



-- Printing


printState : Model -> Int -> String
printState model width =
    let
        { score } =
            getGameInfo model

        scoreMsg =
            "Score : " ++ String.fromInt score

        stateMsg =
            case model of
                Error { msg } ->
                    "ERROR"

                GameOver _ ->
                    "GAME OVER"

                PlayingButPaused _ ->
                    "PRESS SPACE TO PLAY"

                _ ->
                    ""

        spacerLength =
            width
                - String.length scoreMsg
                - String.length stateMsg
                |> max 1
    in
    scoreMsg ++ String.repeat spacerLength " " ++ stateMsg


getTextWidth : String -> Int
getTextWidth text =
    case String.uncons text of
        Nothing ->
            0

        Just ( char, rest ) ->
            if char == '\n' then
                0

            else
                1 + getTextWidth rest



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

        Game.GameOver _ tiles _ ->
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
