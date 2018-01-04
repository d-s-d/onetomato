import Html exposing (Html, div, button, text, pre)
import Html.Attributes exposing (disabled, style)

import Material.Grid exposing (Align(Middle), Device(..), align, cell, grid, offset, size)
import Material
import Material.Options as Options
import Material.Button as Button
import Html.Events exposing (onClick)
import Material.Scheme
import String exposing (padLeft)
import Time exposing (Time, second)

main = Html.program {
    init = init,
    view = view,
    update = update,
    subscriptions = subscriptions}

-- Model

type TimerState = Paused Int | Ticking Int | Stopped

type alias Model = {timerState: TimerState, mdl: Material.Model}

-- Update

type Msg = Tick Time | Start | Pause | Reset | Mdl (Material.Msg Msg)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Mdl msg_ -> Material.update Mdl msg_ model
        _ ->
            ((case model.timerState of
                Ticking s ->
                    (case msg of
                        Tick _ -> let newState = if s > 1 then Ticking (s-1) else Stopped
                            in { model | timerState = newState }
                        Pause -> { model | timerState = Paused s }
                        _ -> model)
                Paused s ->
                    (case msg of
                        Start -> { model | timerState = Ticking s }
                        Reset -> let (a,b) = init in a
                        _ -> { model | timerState = Paused s } )
                Stopped ->
                    (let (a, _) = init in case msg of
                        Start -> a
                        Reset -> a
                        _ -> { model | timerState = Stopped }
                    ))
            , Cmd.none)

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.timerState of
        Ticking s -> Time.every second Tick
        _ -> Sub.none

-- init

init : (Model, Cmd Msg)
init = ({timerState = Paused (25*60), mdl = Material.model}, Cmd.none)

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
        Ticking _ -> btn model Pause "Pause" False
        Paused _ -> btn model Start "Start" False
        Stopped -> btn model Start "Start" True

timeView : Model -> Html Msg
timeView model =
    let (color, s) = case model.timerState of
            Ticking s -> ("black", s)
            Paused s -> ("gray", s)
            Stopped -> ("red", 0)
    in pre [style [("text-align", "center"), ("color", color)]] [text <| minSecText <| minSec s]

minSec : Int -> (Int, Int)
minSec s = (s // 60, s % 60)

minSecText : (Int, Int) -> String
minSecText (m, s) =
    let twoDigit s = padLeft 2 '0' (toString s)
    in twoDigit m ++ ":" ++ (twoDigit s)