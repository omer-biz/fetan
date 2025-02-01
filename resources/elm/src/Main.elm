port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict exposing (Dict, update)
import Dictation as DictGen
import Html exposing (Html, div, main_, p, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class, tabindex)
import Html.Events exposing (onBlur, onFocus, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode exposing (dict)
import Layout exposing (KeyAttempt(..), KeyModifier(..))
import Layouts.SilPowerG as SilPowerG
import Random
import Time


type alias Model =
    { keyboard : Keyboard
    , dictation : Dictation
    , info : Info

    -- seconds since last dictation generated
    , time : Float

    -- seconds since last key down event
    , lastKeyEvent : Float
    }


type alias Info =
    { metrics : Metrics
    , lessonIdx : Int
    }


type alias Metrics =
    { speed : { old : Int, new : Int }
    , accuracy : { old : Int, new : Int }
    , score : { old : Int, new : Int }
    }


type alias Dictation =
    { prev : List Letter
    , current : Letter
    , next : List Letter
    }


type LetterState
    = Fresh
    | Wrong
    | Rolling


type alias Letter =
    { letter : Char
    , state : LetterState
    , wasWrong : Bool -- redundent
    }


type alias Keyboard =
    { focusKeyBr : Bool
    , modifier : Layout.KeyModifier
    , layout : Layout.Layout
    , keys : List Key
    }


type KeyState
    = Pressed
    | Released
    | Hinted


type alias Key =
    { view : String
    , code : String
    , state : KeyState
    }


type Msg
    = NoOp
    | KeyDown String
    | KeyUp String
    | ModKeyDown String
    | ModKeyUp String
    | FocusKeyBr
    | BlurKeyBr
    | NewDict String
    | Tick Time.Posix


port saveInfo : Encode.Value -> Cmd msg


wordCount : number
wordCount =
    2


dictGenerators : Array (DictGen.Nonempty Char)
dictGenerators =
    Array.fromList [ DictGen.consonantOne, DictGen.consonantTwo, DictGen.consonantThree, DictGen.consonantFour, DictGen.all ]


stringToDictation : String -> Dictation
stringToDictation str =
    case String.uncons str of
        -- the "\u{0000}" helps solve the off by one error when checking if the dictation is done
        Just ( curr, next ) ->
            Dictation (lettersFromString "") (Letter curr Fresh False) (lettersFromString (next ++ "\u{0000}"))

        Nothing ->
            Dictation (lettersFromString "?") (Letter '?' Fresh False) (lettersFromString "?")


lettersFromString : String -> List Letter
lettersFromString str =
    str
        |> String.toList
        |> List.map (\l -> Letter l Fresh False)


specialKeys : Dict String String
specialKeys =
    Dict.fromList
        [ ( "Tab", "flex-grow" )
        , ( "Capslock", "w-20" )
        , ( "ShiftLeft", "flex-grow" )
        , ( "ShiftRight", "flex-grow" )
        , ( "ControlLeft", "w-20" )
        , ( "ControlRight", "w-20" )
        , ( "ALT", "w-20" )
        , ( "AltLeft", "w-20" )
        , ( "AltRight", "w-20" )
        , ( "Space", "flex-grow" )
        , ( "Enter", "flex-grow" )
        , ( "Backspace", "flex-grow w-24" )
        , ( "Backslash", "flex-grow" )
        ]


updateFirstOccurrence : (a -> Bool) -> (a -> a) -> List a -> List a
updateFirstOccurrence predicate modVal list =
    let
        helper seen remaining =
            case remaining of
                [] ->
                    List.reverse seen

                x :: xs ->
                    if predicate x then
                        List.reverse seen ++ (modVal x :: xs)

                    else
                        helper (x :: seen) xs
    in
    helper [] list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        keyboard =
            model.keyboard

        curLayout =
            keyboard.layout

        info =
            model.info

        dictation =
            model.dictation

        metrics =
            info.metrics

        updateKey key state =
            updateFirstOccurrence
                (\k -> key == k.code)
                (\k -> { k | state = state })
                keyboard.keys
    in
    case msg of
        FocusKeyBr ->
            ( { model | keyboard = { keyboard | focusKeyBr = True } }, Cmd.none )

        BlurKeyBr ->
            ( { model | keyboard = { keyboard | focusKeyBr = False } }, Cmd.none )

        KeyDown key ->
            ( { model | keyboard = { keyboard | keys = updateKey key Pressed }, lastKeyEvent = 0 }, Cmd.none )

        KeyUp key ->
            let
                ( dict, layout ) =
                    updateDictation key keyboard.modifier keyboard.layout dictation

                nextLessonIdx =
                    if
                        List.isEmpty dict.next
                            == True
                            && metrics.speed.old
                            > 80
                            && metrics.speed.new
                            > 80
                            && metrics.accuracy.new
                            > 80
                            && (info.lessonIdx + 1 < Array.length dictGenerators)
                    then
                        info.lessonIdx + 1

                    else
                        info.lessonIdx
            in
            ( { model
                -- TODO: Change SilPowerG type based on the currently selected layout
                | keyboard = { keyboard | keys = updateKey key Released, layout = layout }
                , dictation = dict
                , info = { info | lessonIdx = nextLessonIdx }
              }
            , if List.isEmpty dict.next then
                dictGenerators
                    |> Array.get nextLessonIdx
                    |> Maybe.withDefault DictGen.consonantOne
                    |> DictGen.genFromList wordCount
                    |> Random.generate NewDict

              else
                Cmd.none
            )

        NewDict dict ->
            let
                allChars =
                    List.concat [ dictation.prev, dictation.current :: dictation.next ]

                lenChars =
                    List.length allChars

                correctChars =
                    allChars
                        |> List.filter (\l -> l.wasWrong == False)
                        |> List.length

                newMetrics =
                    if model.time /= 0 then
                        -- initial run
                        -- TODO: theoretically this could cause a race condition.
                        info.metrics
                            |> updateSpeed model.time lenChars
                            |> updateAccuracy lenChars correctChars
                            |> updateScore

                    else
                        metrics
            in
            ( { model
                | dictation = stringToDictation dict
                , time = 0
                , info = { info | metrics = newMetrics }
              }
            , if model.time == 0 then
                Cmd.none

              else
                saveInfo <| encodeInfo { info | metrics = newMetrics }
            )

        Tick _ ->
            let
                hints =
                    Maybe.map (\h -> h dictation.current.letter curLayout.partial) curLayout.hint
                        |> Maybe.andThen identity
                        |> htoList

                hintedToList =
                    keyboard.keys
                        |> List.map (hintMod hints)

                keys =
                    if model.lastKeyEvent == 5 then
                        hintedToList

                    else
                        keyboard.keys
            in
            ( { model
                | time = model.time + 1
                , lastKeyEvent = model.lastKeyEvent + 1
                , keyboard = { keyboard | keys = keys }
              }
            , Cmd.none
            )

        ModKeyDown key ->
            let
                newState =
                    Layout.keyModDown key keyboard.modifier

                keys =
                    -- every time the user presses the mod keys the printer gets called 47
                    -- times it's either this or storing the key views for plain, Shift, CapsLock, and ShiftCapslock
                    -- the classic tradeoff "storage or cpu". We will see.
                    -- I just hope the layout authors will not write heavy "printers"
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    if k.code == key then
                                        { k | state = Pressed }

                                    else
                                        k

                                else
                                    { k | view = curLayout.printer newState k.code }
                            )
            in
            ( { model
                | keyboard = { keyboard | modifier = newState, keys = keys }
              }
            , Cmd.none
            )

        ModKeyUp key ->
            let
                newState =
                    Layout.keyModUp key keyboard.modifier

                keys =
                    -- same here: read the prev comment
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    { k | state = Released }

                                else
                                    { k | view = curLayout.printer newState k.code }
                            )
            in
            ( { model
                | keyboard = { keyboard | modifier = newState, keys = keys }
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


hintMod : List String -> Key -> Key
hintMod hints key =
    if List.member key.code hints then
        { key | state = Hinted }

    else
        key


bestShiftForKey : String -> String
bestShiftForKey key =
    let
        letter =
            String.dropLeft 3 key

        leftHandKeys =
            [ "Q", "W", "E", "R", "T", "A", "S", "D", "F", "G", "Z", "X", "C", "V", "B" ]
    in
    if List.member letter leftHandKeys then
        "ShiftRight"

    else
        "ShiftLeft"


htoList : Maybe ( KeyModifier, String ) -> List String
htoList hint =
    let
        modToCode mod code =
            case mod of
                NoModifier ->
                    []

                CapsLock ->
                    [ "Capslock" ]

                Shift ->
                    [ bestShiftForKey code ]

                ShiftCapsLock ->
                    [ "ShiftRight", "Capslock" ]
    in
    case hint of
        Just ( mod, code ) ->
            code :: modToCode mod code

        Nothing ->
            []


updateDictation :
    String
    -> Layout.KeyModifier
    -> Layout.Layout
    -> Dictation
    -> ( Dictation, Layout.Layout )
updateDictation codePoint keybrState layout dictation =
    let
        current =
            dictation.current

        advanceDictation =
            case dictation.next of
                newCurr :: next ->
                    { dictation
                        | next = next
                        , current = newCurr
                        , prev = dictation.prev ++ [ dictation.current ]
                    }

                [] ->
                    dictation

        wrongAttempt =
            { dictation | current = { current | state = Wrong, wasWrong = True } }

        rollingCurrent =
            { current | state = Rolling }
    in
    case layout.judge keybrState codePoint dictation.current.letter layout.partial of
        Layout.Partial s ->
            ( { dictation | current = rollingCurrent }, { layout | partial = s } )

        Layout.Correct ->
            ( advanceDictation, { layout | partial = Nothing } )

        Layout.Wrong ->
            ( wrongAttempt, { layout | partial = Nothing } )


updateSpeed : Float -> Int -> Metrics -> Metrics
updateSpeed time lenChars metrics =
    let
        speed =
            { old = metrics.speed.new, new = round <| toFloat lenChars / (time / 60) }
    in
    { metrics | speed = speed }


updateAccuracy : Int -> Int -> Metrics -> Metrics
updateAccuracy totalChars correctChars metrics =
    let
        accuracy =
            { old = metrics.accuracy.new, new = round <| (toFloat correctChars * 100) / toFloat totalChars }
    in
    { metrics | accuracy = accuracy }


updateScore : Metrics -> Metrics
updateScore metrics =
    let
        score =
            { old = metrics.score.new, new = metrics.score.old + metrics.speed.new + metrics.accuracy.new }
    in
    { metrics | score = score }


view : Model -> Html Msg
view model =
    main_ [ class "text-white flex items-center justify-center h-screen flex-col" ]
        [ div []
            [ viewInfo model.info
            , viewDictation model.dictation
            , viewKeyBoard model.keyboard
            ]
        ]


viewInfo : Info -> Html Msg
viewInfo info =
    table [ class "font-mono mb-8" ]
        [ tbody []
            [ viewMetrics info.metrics
            , tr [] [ td [ class "pr-2 text-right" ] [ text "Current Keys:" ], viewCurrentKeys info.lessonIdx ]
            ]
        ]


viewCurrentKeys : Int -> Html msg
viewCurrentKeys idx =
    let
        viewK k =
            span [ class "px-1" ] [ text <| String.fromChar k ]
    in
    dictGenerators
        |> Array.get idx
        |> Maybe.withDefault DictGen.consonantOne
        |> DictGen.toList
        |> List.map viewK
        |> td [ class "font-am" ]


viewMetrics : Metrics -> Html msg
viewMetrics metrics =
    let
        viewOld m pst =
            if m.new >= m.old then
                span [ class "text-green-400" ] [ text <| "+" ++ String.fromInt (m.new - m.old) ++ pst ]

            else
                span [ class "text-red-400" ] [ text <| String.fromInt (m.new - m.old) ++ pst ]

        viewMetric m pst =
            span [] [ span [] [ text <| String.fromInt m.new ++ pst ++ "(" ], viewOld m pst, span [] [ text ")" ] ]
    in
    tr []
        [ td [ class "pr-2 text-right" ]
            [ text "Metrics:" ]
        , td [ class "space-x-2" ]
            [ span [] [ text "Speed: ", viewMetric metrics.speed "wpm" ]
            , span [] [ text "Accuracy: ", viewMetric metrics.accuracy "%" ]
            , span [] [ text "Score: ", viewMetric metrics.score "" ]
            ]
        ]


viewDictation : Dictation -> Html msg
viewDictation dict =
    let
        currentKeyStyle =
            if
                (dict.current.wasWrong && (dict.current.state /= Rolling))
                    || dict.current.state
                    == Wrong
            then
                "text-red-400"

            else if dict.current.state == Rolling then
                "text-yellow-300"

            else
                ""

        viewLetter lt =
            let
                wasWrong =
                    if lt.wasWrong then
                        "text-red-400"

                    else
                        ""
            in
            span [ class wasWrong ]
                [ text <| (lt.letter |> String.fromChar |> String.replace " " " · ") ]

        viewCurrentLetter =
            span [ class "border-white border-b-4" ]
                [ text <| (dict.current.letter |> String.fromChar |> String.replace " " " · ") ]

        viewLetters lts =
            List.map viewLetter lts
    in
    div [ class "mx-auto border rounded border-2 border-white p-4 mb-4 max-w-[800px] text-3xl font-normal leading-relaxed" ]
        [ p [ class "inline m-0 p-0 text-gray-400" ] (viewLetters dict.prev)
        , p [ class <| String.join " " [ "inline m-0 p-0", currentKeyStyle ] ]
            [ viewCurrentLetter ]
        , p [ class "inline m-0 p-0" ] (viewLetters dict.next)
        ]


keyDown : msg -> Html.Attribute msg
keyDown msg =
    preventDefaultOn "keydown" <|
        Decode.map (\a -> ( a, True )) (Decode.succeed msg)


keyUp : msg -> Html.Attribute msg
keyUp msg =
    preventDefaultOn "keyup" <|
        Decode.map (\a -> ( a, True )) (Decode.succeed msg)


viewKeyBoard : Keyboard -> Html Msg
viewKeyBoard keyboard =
    let
        isfocused =
            if keyboard.focusKeyBr == False then
                div [ class "absolute z-20 inset-0 bg-white bg-opacity-5 backdrop-blur-sm flex items-center justify-center cursor-pointer" ]
                    [ span [ class "text-lg font-semibold text-gray-100" ] [ text "Click to Start" ] ]

            else
                text ""

        firstRow =
            List.take 14 keyboard.keys
                |> List.map viewKey
                |> viewRow

        secondRow =
            List.drop 14 keyboard.keys
                |> List.take 13
                |> List.map viewKey
                |> viewRow

        thirdRow =
            List.drop 27 keyboard.keys
                |> List.take 12
                |> List.map viewKey
                |> viewRow

        fourthRow =
            List.drop 39 keyboard.keys
                |> List.take 5
                |> List.map viewKey
                |> viewRow
    in
    div
        [ class <| "border-2 p-6 rounded border-gray-500 relative"
        , onFocus FocusKeyBr
        , onBlur BlurKeyBr
        , tabindex 0 -- Helps make a div focusable and blurable.
        , keyDown NoOp
        , keyUp NoOp
        ]
        [ firstRow
        , secondRow
        , thirdRow
        , fourthRow
        , isfocused
        ]


viewRow : List (Html msg) -> Html msg
viewRow row =
    div [ class "flex gap-1 py-1" ] row


viewKey : Key -> Html msg
viewKey key =
    let
        bg =
            case key.state of
                Pressed ->
                    "bg-lime-500"

                Released ->
                    "bg-gray-600"

                Hinted ->
                    "bg-gray-300 text-black animate-pulse"

        extraStyle =
            Dict.get key.code specialKeys
                |> Maybe.withDefault ""
    in
    div
        [ class <|
            String.join
                " "
                [ "relative z-10 x-4 py-2 text-center rounded shadow font-semibold w-12", bg, extraStyle ]
        ]
        [ text key.view
        , if key.code == "KeyF" || key.code == "KeyJ" then
            span [ class "absolute z-2 bottom-0 inset-x-0 text-2xl" ] [ text "." ]

          else
            text ""
        , case String.split "Key" key.code of
            "" :: l :: [] ->
                span [ class "absolute z-2 top-0 left-1 text-xs font-normal" ] [ text l ]

            _ ->
                text ""
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.keyboard.focusKeyBr then
        Sub.batch
            [ onKeyDown <| Decode.map dispatchDown keyDecoder
            , onKeyUp <| Decode.map dispatchUp keyDecoder
            , Time.every 1000 Tick
            ]

    else
        Sub.none


modifierKeys : List String
modifierKeys =
    [ "ShiftLeft", "ShiftRight", "Capslock", "ControlRight", "ControlLeft", "AltRight", "AltLeft", "Tab", "MetaLeft", "MetaRight", "Enter" ]


dispatchHelper : (String -> Msg) -> (String -> Msg) -> String -> Msg
dispatchHelper modMsg regularMsg key =
    if List.member key modifierKeys then
        modMsg key

    else
        regularMsg key


dispatchDown : String -> Msg
dispatchDown =
    dispatchHelper ModKeyDown KeyDown


dispatchUp : String -> Msg
dispatchUp =
    dispatchHelper ModKeyUp KeyUp


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.map (\code -> code) <|
        Decode.field "code" Decode.string


tab : Key
tab =
    Key "⇥" "Tab" Released


capslock : Key
capslock =
    Key "⇪" "Capslock" Released


enter : Key
enter =
    Key "⏎" "Enter" Released


shiftLeft : Key
shiftLeft =
    Key "⇧" "ShiftLeft" Released


shiftRight : Key
shiftRight =
    Key "⇧" "ShiftRight" Released


altLeft : Key
altLeft =
    Key "alt" "AltLeft" Released


altRight : Key
altRight =
    Key "alt" "AltRight" Released


ctrlLeft : Key
ctrlLeft =
    Key "ctrl" "ControlLeft" Released


ctrlRight : Key
ctrlRight =
    Key "ctrl" "ControlRight" Released


space : Key
space =
    Key " " "Space" Released


insertAt : Int -> a -> List a -> List a
insertAt index obj lst =
    if index <= 0 then
        obj :: lst

    else
        case lst of
            [] ->
                [ obj ]

            x :: xs ->
                x :: insertAt (index - 1) obj xs


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        curLayout =
            SilPowerG.layout

        keys =
            tab
                :: (Layout.codePoints
                        |> List.map (\e -> Key (curLayout.printer Layout.NoModifier e) e Released)
                   )

        withModKeys =
            (insertAt 14 capslock keys
                |> insertAt 26 enter
                |> insertAt 27 shiftLeft
                |> insertAt 39 shiftRight
            )
                ++ [ ctrlLeft, altLeft, space, altRight, ctrlRight ]

        keyboard =
            Keyboard False Layout.NoModifier curLayout withModKeys

        info =
            case Decode.decodeValue infoDecoder flags of
                Ok m ->
                    m

                Err _ ->
                    Info initMetric 0

        model =
            Model keyboard (stringToDictation "") info 0 0

        dictation =
            dictGenerators
                |> Array.get info.lessonIdx
                |> Maybe.withDefault DictGen.consonantOne
                |> DictGen.genFromList wordCount
    in
    ( model, Random.generate NewDict dictation )


metricDecoder : Decode.Decoder { old : Int, new : Int }
metricDecoder =
    Decode.map2 (\o n -> { old = o, new = n })
        (Decode.field "old" Decode.int)
        (Decode.field "new" Decode.int)


metricsDecoder : Decode.Decoder Metrics
metricsDecoder =
    Decode.map3 Metrics
        (Decode.field "speed" metricDecoder)
        (Decode.field "accuracy" metricDecoder)
        (Decode.field "score" metricDecoder)


infoDecoder : Decode.Decoder Info
infoDecoder =
    Decode.map2 Info
        (Decode.field "metrics" metricsDecoder)
        (Decode.field "lessonIdx" Decode.int)


encodeMetric : { old : Int, new : Int } -> Encode.Value
encodeMetric metric =
    Encode.object [ ( "old", Encode.int metric.old ), ( "new", Encode.int metric.new ) ]


encodeMetrics : Metrics -> Encode.Value
encodeMetrics metrics =
    Encode.object
        [ ( "speed", encodeMetric metrics.speed )
        , ( "accuracy", encodeMetric metrics.accuracy )
        , ( "score", encodeMetric metrics.score )
        ]


encodeInfo : Info -> Encode.Value
encodeInfo info =
    Encode.object
        [ ( "metrics", encodeMetrics info.metrics )
        , ( "lessonIdx", Encode.int info.lessonIdx )
        ]


initMetric : Metrics
initMetric =
    let
        new =
            { old = 0, new = 0 }
    in
    Metrics new new new


main : Program Encode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
