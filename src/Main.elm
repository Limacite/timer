port module Main exposing (Model, Msg(..), beep, init, main, subscription, update, view)

import Browser
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes exposing (autoplay, controls, preload, src)
import Task
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , subscriptions = subscription
        , update = update
        }


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , counter : Int
    , limit : Int
    , interval : Int
    , working : Bool
    , inStart : Bool
    }



--INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) 0 3 2 True False
    , Task.perform AdjustTimeZone Time.here
    )


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | DoTimer
    | ChangePlayer
    | IncrementLimit
    | DecrementLimit
    | IncrementInterval
    | DecrementInterval
    | EditLimit String
    | EditInterval String



--UPDATE


port beep : () -> Cmd msg


port timeSet : ( Int, Int, Int ) -> Cmd msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                c =
                    if model.inStart then
                        if model.counter < (model.limit + model.interval) then
                            model.counter + 1

                        else if model.working then
                            0

                        else
                            0

                    else
                        model.counter

                w =
                    if model.inStart then
                        if model.counter + 1 < model.limit then
                            True

                        else if model.counter < model.limit + model.interval then
                            False

                        else
                            True

                    else
                        model.working

                cmd =
                    if model.inStart then
                        if model.counter < (model.limit + model.interval) then
                            Cmd.none

                        else
                            beep ()

                    else
                        Cmd.none
            in
            ( { model | counter = c, working = w }, cmd )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )

        DoTimer ->
            let
                r =
                    if model.inStart then
                        False

                    else
                        True
            in
            ( { model | inStart = r }, Cmd.none )

        ChangePlayer ->
            let
                cmd =
                    timeSet ( model.limit, model.interval, 0 )
            in
            ( { model | counter = 0, working = True }, cmd )

        IncrementLimit ->
            ( { model | limit = model.limit + 1 }, timeSet ( model.limit + 1, model.interval, model.counter ) )

        DecrementLimit ->
            ( { model
                | limit =
                    if model.limit > 0 then
                        model.limit - 1

                    else
                        model.limit
              }
            , timeSet ( model.limit - 1, model.interval, model.counter )
            )

        IncrementInterval ->
            ( { model | interval = model.interval + 1 }, timeSet ( model.limit, model.interval + 1, model.counter ) )

        DecrementInterval ->
            ( { model
                | interval =
                    if model.interval > 0 then
                        model.interval - 1

                    else
                        model.interval
              }
            , timeSet ( model.limit, model.interval - 1, model.counter )
            )

        EditLimit inputedTime ->
            let
                t =
                    case String.toInt inputedTime of
                        Nothing ->
                            model.limit

                        Just int ->
                            int

                cmd =
                    case String.toInt inputedTime of
                        Nothing ->
                            Cmd.none

                        Just int ->
                            timeSet ( int, model.interval, model.counter )
            in
            ( { model
                | limit =
                    if t > 0 then
                        t

                    else
                        model.limit
              }
            , cmd
            )

        EditInterval inputedTime ->
            let
                t =
                    case String.toInt inputedTime of
                        Nothing ->
                            model.interval

                        Just int ->
                            int

                cmd =
                    case String.toInt inputedTime of
                        Nothing ->
                            Cmd.none

                        Just int ->
                            timeSet ( model.limit, int, model.counter )
            in
            ( { model
                | interval =
                    if t > 0 then
                        t

                    else
                        model.limit
              }
            , cmd
            )


subscription : Model -> Sub Msg
subscription model =
    Time.every 1000 Tick



--VIEW


view : Model -> Html.Html Msg
view model =
    let
        w =
            String.fromInt
                (if model.working then
                    model.counter

                 else
                    model.limit
                )

        b =
            String.fromInt
                (if model.working then
                    0

                 else
                    model.counter - model.limit
                )

        c =
            String.fromInt model.counter

        l =
            String.fromInt model.limit

        i =
            String.fromInt model.interval

        bt =
            if model.inStart then
                "Stop!"

            else
                "Start"

        btClass =
            if model.inStart then
                "bt stop-bt"

            else
                "bt"
    in
    layout [] <|
        row [ centerX, centerY, spacing 30 ]
            [ column []
                [ el [ centerX, Font.size 50 ] <| text ((w ++ "/") ++ l)
                , el [ centerX, Font.size 50 ] <| text ((b ++ "/") ++ i)
                , row [ spacing 10 ]
                    [ el [ BD.color (rgb255 0 0 0), BD.width 1, BD.solid ] <| Input.button [] { onPress = Just DoTimer, label = text bt }
                    , el [ BD.color (rgb255 0 0 0), BD.width 1, BD.solid ] <| Input.button [] { onPress = Just ChangePlayer, label = text "Reset" }
                    ]
                ]
            , column [ height fill, width (px 90) ]
                [ el [ centerX ] <| text "work time"
                , row [ centerY, width fill ]
                    [ el [ width fill ] <| Input.text [ width (px 75) ] { onChange = EditLimit, text = l, placeholder = Nothing, label = Input.labelAbove [] <| text "" }
                    , column [ height fill ]
                        [ Input.button [ alignTop ] { onPress = Just IncrementLimit, label = text "Λ" }
                        , Input.button [ alignBottom ] { onPress = Just DecrementLimit, label = text "V" }
                        ]
                    ]
                ]
            , column [ height fill, width (px 90) ]
                [ el [ centerX ] <| text "interval"
                , row [ centerY, width fill ]
                    [ el [ width fill ] <| Input.text [ height fill, width (px 75) ] { onChange = EditInterval, text = i, placeholder = Nothing, label = Input.labelAbove [] <| text "" }
                    , column [ height fill, centerY ]
                        [ Input.button [ alignTop ] { onPress = Just IncrementInterval, label = text "Λ" }
                        , Input.button [ alignBottom ] { onPress = Just DecrementInterval, label = text "V" }
                        ]
                    ]
                ]
            ]
