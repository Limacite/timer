module Main exposing (Model, Msg(..), init, main, subscription, update, view)

import Browser
import Element exposing (..)
import Element.Background as BG
import Element.Border as BD
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
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
    , counter1 : Int
    , counter2 : Int
    , limit : Int
    , interval : Int
    , working : Bool
    , inStart : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) 0 0 3 2 True False
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                w =
                    ((model.counter1 + 1) < model.limit) && (model.counter2 < model.interval)

                c1 =
                    if model.inStart && model.working then
                        if model.counter1 < model.limit then
                            model.counter1 + 1

                        else
                            0

                    else
                        model.counter1

                c2 =
                    if model.inStart && not model.working then
                        if not model.working then
                            model.counter2 + 1

                        else
                            0

                    else
                        model.counter2
            in
            ( { model | counter1 = c1, counter2 = c2, working = w }, Cmd.none )

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
            ( { model | counter1 = 0, counter2 = 0 }, Cmd.none )

        IncrementLimit ->
            ( { model | limit = model.limit + 1 }, Cmd.none )

        DecrementLimit ->
            ( { model | limit = model.limit - 1 }, Cmd.none )

        IncrementInterval ->
            ( { model | interval = model.interval + 1 }, Cmd.none )

        DecrementInterval ->
            ( { model | interval = model.interval - 1 }, Cmd.none )

        EditLimit inputedTime ->
            let
                t =
                    case String.toInt inputedTime of
                        Nothing ->
                            model.limit

                        Just int ->
                            int
            in
            ( { model | limit = t }, Cmd.none )

        EditInterval inputedTime ->
            let
                t =
                    case String.toInt inputedTime of
                        Nothing ->
                            model.interval

                        Just int ->
                            int
            in
            ( { model | limit = t }, Cmd.none )



{- ChangeLimit newtime ->
   ( { model | limit = model.limit }, Cmd.none )
-}


subscription : Model -> Sub Msg
subscription model =
    Time.every 1000 Tick


view : Model -> Html Msg
view model =
    let
        w =
            String.fromInt model.counter1

        b =
            String.fromInt model.counter2

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
                    [ el [ width fill ] <| Input.text [ width (px 75) ] { onChange = EditLimit, text = "", placeholder = Just (Input.placeholder [] <| text l), label = Input.labelAbove [] <| text "" }
                    , column [ height fill ]
                        [ Input.button [ alignTop ] { onPress = Just IncrementLimit, label = text "Λ" }
                        , Input.button [ alignBottom ] { onPress = Just DecrementLimit, label = text "V" }
                        ]
                    ]
                ]
            , column [ height fill, width (px 90) ]
                [ el [ centerX ] <| text "interval"
                , row [ centerY, width fill ]
                    [ el [ width fill ] <| Input.text [ height fill, width (px 75) ] { onChange = EditInterval, text = "", placeholder = Just (Input.placeholder [] <| text i), label = Input.labelAbove [] <| text "" }
                    , column [ height fill, centerY ]
                        [ Input.button [ alignTop ] { onPress = Just IncrementInterval, label = text "Λ" }
                        , Input.button [ alignBottom ] { onPress = Just DecrementInterval, label = text "V" }
                        ]
                    ]
                ]
            ]
