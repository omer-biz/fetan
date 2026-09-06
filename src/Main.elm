module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Browser.Navigation as Nav
import Community
import Dict exposing (Dict)
import Dictation as DictGen
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Layout as Layout exposing (Layout(..))
import Ports
import Random
import Stats exposing (LetterStat, SessionRecord)
import Time
import Types.KeyModifier exposing (KeyModifier(..))
import Types.Core exposing (..)
import Task
import Types.Msg exposing (..)
import Url exposing (Url)

import Storage
import DictationLogic exposing (..)
import Update exposing (update, modifierKeys)
import View exposing (view)


init : Encode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        info =
            case Decode.decodeValue (Decode.field "lessonInfo" Storage.infoDecoder) flags of
                Ok m ->
                    m

                Err _ ->
                    case Decode.decodeValue Storage.infoDecoder flags of
                        Ok m ->
                            m

                        Err _ ->
                            Info initMetric "GeezIME" [] Dict.empty { totalDuration = 0, totalSessions = 0, topWpm = 0, topAccuracy = 0, sumWpm = 0, sumAccuracy = 0 }

        curLayoutKind =
            layoutKindFromString info.layoutKind

        curLayout =
            Layout.initLayout curLayoutKind

        keys =
            tab
                :: (Layout.codePoints
                        |> List.map (\e -> Key (Layout.render NoModifier e curLayout) e Released)
                   )

        withModKeys =
            (insertAt 14 capslock keys
                |> insertAt 26 enter
                |> insertAt 27 shiftLeft
                |> insertAt 39 shiftRight
            )
                ++ [ ctrlLeft, altLeft, space, altRight, ctrlRight ]

        keyboard =
            Keyboard False NoModifier withModKeys

        nowTime =
            case Decode.decodeValue (Decode.field "now" Decode.float) flags of
                Ok t ->
                    t

                Err _ ->
                    0

        themeStr =
            case Decode.decodeValue (Decode.field "theme" Decode.string) flags of
                Ok "light" ->
                    Light

                _ ->
                    Dark

        model =
            { keyboard = keyboard
            , dictation = stringToDictation ""
            , info = info
            , time = 0
            , timeOrigin = nowTime
            , zone = Time.utc
            , sessionStartTime = 0
            , currentTime = nowTime
            , lastSuccessTime = 0
            , lastKeyEvent = 0
            , currentLayout = curLayout
            , layoutKind = curLayoutKind
            , started = False
            , theme = themeStr
            , currentErrors = []
            , navKey = navKey
            , route = routeFromUrl url
            , hoveringStats = []
            , hoveringMastery = []
            , communityData = Community.init
            , justLeveledUp = False
            }

        dictation =
            DictGen.genForLevel (getCurrentLayoutData info).lessonIdx
    in
    ( model
    , Cmd.batch
        [ Random.generate NewDict dictation
        , Task.perform GotTimeZone Time.here
        , if model.route == CommunityRoute then
            Ports.fetchCommunityStats ()

          else
            Cmd.none
        ]
    )

initMetric : Metrics
initMetric =
    let
        new =
            { old = 0, new = 0 }
    in
    Metrics new new

main : Program Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        kbSubs =
            if model.keyboard.focusKeyBr || model.route == TypingRoute then
                Sub.batch
                    [ Time.every 1000 Tick
                    ]
            else
                Sub.none
    in
    if model.route == CommunityRoute then
        Sub.batch [ Ports.receiveCommunityStats GotCommunityStats ]
    else
        Sub.batch [ kbSubs, Ports.receiveCommunityStats GotCommunityStats ]


dispatchHelper : (String -> Msg) -> (KeyEvent -> Msg) -> KeyEvent -> Msg
dispatchHelper modMsg regularMsg key =
    if List.member key.code modifierKeys then
        modMsg key.code

    else
        regularMsg key

dispatchDown : KeyEvent -> Msg
dispatchDown =
    dispatchHelper ModKeyDown KeyDown

dispatchUp : KeyEvent -> Msg
dispatchUp =
    dispatchHelper ModKeyUp KeyUp

layoutKindFromString : String -> Layout.LayoutKind
layoutKindFromString str =
    case str of
        "SilPowerG" ->
            Layout.SilPowerG

        "PowerGeez" ->
            Layout.PowerGeez

        "GeezIME" ->
            Layout.GeezIME

        _ ->
            Layout.GeezIME

-- Temporary definition of Keys to satisfy compilation, these should be in a Core file ideally.
tab = Key "⇥" "Tab" Released
capslock = Key "⇪" "CapsLock" Released
enter = Key "⏎" "Enter" Released
shiftLeft = Key "⇧" "ShiftLeft" Released
shiftRight = Key "⇧" "ShiftRight" Released
altLeft = Key "alt" "AltLeft" Released
altRight = Key "alt" "AltRight" Released
ctrlLeft = Key "ctrl" "ControlLeft" Released
ctrlRight = Key "ctrl" "ControlRight" Released
space = Key " " "Space" Released

insertAt : Int -> a -> List a -> List a
insertAt index obj lst =
    case lst of
        [] ->
            [ obj ]

        x :: xs ->
            if index == 0 then
                obj :: x :: xs
            else
                x :: insertAt (index - 1) obj xs
