import Html exposing (Html, div, button, text, pre)
import Html.Attributes exposing (disabled, style)
import Html.Events exposing (keyCode)

import Json.Decode exposing (andThen)
import Material.Grid exposing (Align(Middle), Device(..), align, cell, grid, offset, size)
import Material.Textfield as Textfield
import Material
import Material.Options as Options exposing (css)
import Material.Button as Button
import Html.Events exposing (onClick)
import Material.Scheme
import Regex exposing (Regex, contains, regex)
import String exposing (padLeft)
import Time exposing (Time, second)

main = Html.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions}

-- Model

type TimerState = Paused | Ticking | Editing Bool

type alias Model = {time: Int, timerState: TimerState, mdl: Material.Model}

-- Update

type Msg = Tick Time
    | Start
    | Pause
    | Reset
    | ValidationError
    | Mdl (Material.Msg Msg)
    | StartEdit
    | StopEdit
    | SetTime Int
    | Nothing

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Mdl msg_ -> Material.update Mdl msg_ model
        SetTime s -> ({ model | time = s, timerState = Editing True }, Cmd.none)
        _ ->
            ((case model.timerState of
                Ticking ->
                    (case msg of
                        Tick _ -> let s = model.time in let newState = if s > 1 then Ticking else Paused
                            in { model | timerState = newState, time = s-1 }
                        Pause -> { model | timerState = Paused }
                        _ -> model)
                Paused ->
                    (case msg of
                        StartEdit -> { model | timerState = Editing True }
                        Start -> { model | timerState = Ticking }
                        Reset -> let (a,b) = init in a
                        _ -> { model | timerState = Paused } )
                Editing valid ->
                    (case msg of
                        StopEdit -> { model | timerState = Paused }
                        ValidationError -> { model | timerState = Editing False }
                        _ -> model))
            , Cmd.none)

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timerState of
        Ticking -> Time.every second Tick
        _ -> Sub.none

-- init

init : (Model, Cmd Msg)
init = ({time = 25*60, timerState = Paused, mdl = Material.model}, Cmd.none)

-- view

type alias Mdl = Material.Model

view : Model -> Html Msg
view model =
    Options.div [] [
        grid [] [ cell [ size All 12, Options.center ] [ timeView model ] ]
        , grid [] [ cell [ size All 12, Options.center ] [ startButton model, btn model Reset "Reset" False ] ]
    ] |>  Material.Scheme.top


btn : Model -> Msg -> String -> Bool -> Html Msg
btn model msg txt d =
        let dfltoptions = [ Button.raised, Options.onClick msg, Options.disabled d ]
        in Button.render Mdl [0] model.mdl dfltoptions [ text txt ]

startButton : Model -> Html Msg
startButton model = case model.timerState of
        Ticking -> btn model Pause "Pause" False
        Paused -> btn model Start "Start" False
        Editing _ -> btn model Start "Start" True

timeView : Model -> Html Msg
timeView model =
    case model.timerState of
        Editing valid -> let s = model.time in
            Textfield.render Mdl [0] model.mdl [
                css "color" (if valid then "black" else "red"),
                Options.onInput (parseTime),
                Options.on "keydown" (Json.Decode.andThen isEnter keyCode) ]
            [ seconds2Text s ]
        _ -> let t = model.time in let (color, s) = case model.timerState of
                Paused -> ("gray", t)
                _ -> ("black", t)
        in
            pre
                [style [("text-align", "center"), ("color", color)], Html.Events.onClick StartEdit]
                [text <| seconds2Text s]

isEnter: number -> Json.Decode.Decoder Msg
isEnter n = if n == 13 then Json.Decode.succeed <| StopEdit else Json.Decode.fail "not Enter"

seconds2Text: Int -> String
seconds2Text = minSec >> minSecText

minSec : Int -> (Int, Int)
minSec s = (s // 60, s % 60)

minSecText : (Int, Int) -> String
minSecText (m, s) =
    let twoDigit s = padLeft 2 '0' (toString s)
    in twoDigit m ++ ":" ++ (twoDigit s)

timeRgx : Regex
timeRgx = regex "^\\d{2}:[0-5][0-9]$"

containsTime : String -> Bool
containsTime s = contains timeRgx s

parseTime : String -> Msg
parseTime s = if containsTime s then
    SetTime <| List.foldl (+) 0 (List.indexedMap (\i v -> (((1-i)*59)+1) * (Result.withDefault 0 <| String.toInt v)) <|
        String.split ":" s)
    else ValidationError