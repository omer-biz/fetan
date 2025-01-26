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
import Layout
import Random
import Time


type alias Model =
    { keyboard : Keyboard
    , dictation : Dictation
    , info : Info

    -- seconds since last dictation generated
    , time : Float
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
    , try : Maybe Char
    , done : Bool
    }


type LetterState
    = New
    | Wrong
    | Rolling


type alias Letter =
    { letter : Char
    , state : LetterState
    , wasWrong : Bool
    }


type alias Keyboard =
    { focusKeyBr : Bool
    , isShiftPressed : Bool
    , keys : List (List Key)
    }


type KeyStatus
    = Pressed
    | Released


type alias Key =
    { view : String
    , altView : String
    , code : String
    , status : KeyStatus
    , form : KeyType
    }


type KeyType
    = Normal
    | Special { extraStyle : String }


type Msg
    = NoOp
    | KeyDown String
    | KeyUp String
    | FocusKeyBr
    | BlurKeyBr
    | NewDict String
    | Tick Time.Posix


port saveInfo : Encode.Value -> Cmd msg


wordCount : number
wordCount =
    15


normalizeLetter : Char -> ( Char, Maybe Char )
normalizeLetter letter =
    let
        cl =
            Char.toCode letter

        vowelOffset =
            modBy 0x08 <| modBy 0x10 cl

        vowelPart =
            if vowelOffset > 0 && vowelOffset < 8 then
                Just <| Char.fromCode (0x12A0 + vowelOffset)

            else
                Nothing

        helper =
            if modBy 0x10 cl >= 8 then
                ((cl // 0x10) * 0x10) + 8

            else
                (cl // 0x10) * 0x10
    in
    ( Char.fromCode helper, vowelPart )


dictGenerators : Array (DictGen.Nonempty Char)
dictGenerators =
    Array.fromList [ DictGen.consonantOne, DictGen.consonantTwo, DictGen.consonantThree, DictGen.consonantFour ]


stringToDictation : String -> Dictation
stringToDictation str =
    case String.uncons str of
        Just ( curr, next ) ->
            Dictation (lettersFromString "") (Letter curr New False) (lettersFromString next) Nothing False

        Nothing ->
            Dictation (lettersFromString "?") (Letter '?' New False) (lettersFromString "?") Nothing False


lettersFromString : String -> List Letter
lettersFromString str =
    str
        |> String.toList
        |> List.map (\l -> Letter l New False)


specialKeys : Dict String String
specialKeys =
    Dict.fromList
        [ ( "Tab", "flex-grow" )
        , ( "CapsLock", "w-24" )
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


toKey : ( String, String, String ) -> Key
toKey ( v, a, c ) =
    specialKeys
        |> Dict.get c
        |> Maybe.andThen (\s -> Just (Key v a c Released <| Special { extraStyle = s }))
        |> Maybe.withDefault (Key v a c Released Normal)


createList : List ( String, String, String ) -> List Key
createList keyList =
    keyList
        |> List.map toKey


layoutToList : List (List ( String, String, String )) -> List (List Key)
layoutToList layout =
    layout
        |> List.map createList


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    let
        helper remainingList =
            case remainingList of
                [] ->
                    Nothing

                x :: xs ->
                    if predicate x then
                        Just x

                    else
                        helper xs
    in
    helper list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        keyboard =
            model.keyboard

        info =
            model.info

        dictation =
            model.dictation

        metrics =
            info.metrics
    in
    case msg of
        FocusKeyBr ->
            ( { model | keyboard = { keyboard | focusKeyBr = True } }, Cmd.none )

        BlurKeyBr ->
            ( { model | keyboard = { keyboard | focusKeyBr = False } }, Cmd.none )

        KeyDown key ->
            let
                updatedKeys =
                    updateKeyStatus Pressed key model.keyboard.keys

                shiftDown =
                    if key == "ShiftLeft" || key == "ShiftRight" then
                        True

                    else
                        model.keyboard.isShiftPressed
            in
            ( { model
                | keyboard =
                    { keyboard
                        | keys = updatedKeys
                        , isShiftPressed = shiftDown
                    }
              }
            , Cmd.none
            )

        KeyUp key ->
            let
                updated =
                    updateKeyStatus Released key keyboard.keys

                shiftUp =
                    if key == "ShiftLeft" || key == "ShiftRight" then
                        False

                    else
                        keyboard.isShiftPressed

                altViewOrView : Key -> Bool
                altViewOrView k =
                    k.code == key

                extractLetter k =
                    if keyboard.isShiftPressed then
                        k.altView

                    else
                        k.view

                acctualLetter =
                    keyboard.keys
                        |> List.concat
                        |> find altViewOrView
                        |> Maybe.map extractLetter

                updatedDict =
                    case acctualLetter of
                        Just tryKey ->
                            case String.uncons tryKey of
                                Just ( tk, "" ) ->
                                    updateDictState tk model.dictation

                                _ ->
                                    model.dictation

                        _ ->
                            model.dictation

                nextLessonIdx =
                    if
                        updatedDict.done
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

                genNewDict =
                    if updatedDict.done == True then
                        dictGenerators
                            |> Array.get nextLessonIdx
                            |> Maybe.withDefault DictGen.consonantOne
                            |> DictGen.genFromList wordCount
                            |> Random.generate NewDict

                    else
                        Cmd.none
            in
            ( { model
                | keyboard =
                    { keyboard
                        | keys = updated
                        , isShiftPressed = shiftUp
                    }
                , dictation = updatedDict
                , info = { info | lessonIdx = nextLessonIdx }
              }
            , genNewDict
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
            ( { model | time = model.time + 1 }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


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


updateDictState : Char -> Dictation -> Dictation
updateDictState tryKey dict =
    let
        current =
            dict.current

        toNextChar =
            case dict.next of
                newCurr :: next ->
                    { dict
                        | next = next
                        , current = newCurr
                        , prev = dict.prev ++ [ current ]
                    }

                [] ->
                    { dict | done = True }

        tkUnicode =
            Char.toCode tryKey

        clUnicode =
            Char.toCode current.letter

        wrongAttempt st =
            { dict | try = Nothing, current = { current | state = st, wasWrong = True } }
    in
    if tryKey == current.letter then
        toNextChar

    else if tkUnicode >= 0x12A1 && tkUnicode <= 0x12A7 then
        -- if it's a vowel
        case dict.try of
            Just incomplete ->
                if Char.toCode incomplete + (tkUnicode - 0x12A0) == clUnicode then
                    toNextChar

                else
                    wrongAttempt Wrong

            Nothing ->
                wrongAttempt Wrong

    else if (clUnicode - tkUnicode) > 0 && (clUnicode - tkUnicode) <= 7 then
        { dict
            | try = Just tryKey
            , current = { current | state = Rolling }
        }

    else
        wrongAttempt Wrong


updateKeyStatus : KeyStatus -> String -> List (List Key) -> List (List Key)
updateKeyStatus s key keys =
    let
        findKey k =
            if k.code == key then
                { k | status = s }

            else
                k

        modKey row =
            List.map findKey row
    in
    List.map modKey keys


view : Model -> Html Msg
view model =
    main_ [ class "text-white flex items-center justify-center h-screen flex-col" ]
        [ div []
            [ viewInfo model.info model.dictation.current.letter
            , viewDictation model.dictation
            , viewKeyBoard model.keyboard
            ]
        ]


viewInfo : Info -> Char -> Html Msg
viewInfo info curr =
    table [ class "font-mono mb-8" ]
        [ tbody []
            [ viewMetrics info.metrics
            , tr [] [ td [ class "pr-2 text-right" ] [ text "Current Keys:" ], viewCurrentKeys info.lessonIdx ]
            , tr [] [ td [ class "pr-2 text-right" ] [ text "Key Combo:" ], viewHint curr ]
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


viewHint : Char -> Html msg
viewHint curr =
    let
        helper ( cons, vowl ) =
            case vowl of
                Just v ->
                    [ text <|
                        String.fromChar curr
                            ++ " = '"
                            ++ String.fromChar cons
                            ++ " + "
                            ++ String.fromChar v
                            ++ "'"
                    ]

                Nothing ->
                    [ text <| "'" ++ String.fromChar cons ++ "'" ]
    in
    span [ class "font-am font-bold underline" ] (helper <| normalizeLetter curr)


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
    in
    div
        [ class <| "border-2 p-6 rounded border-gray-500 relative"
        , onFocus FocusKeyBr
        , onBlur BlurKeyBr
        , tabindex 0 -- Helps make a div focusable and blurable.
        , keyDown NoOp
        , keyUp NoOp
        ]
        (keyboard.keys
            |> List.map (viewRow keyboard.isShiftPressed)
            |> (::) isfocused
        )


viewRow : Bool -> List Key -> Html Msg
viewRow shiftOn row =
    div [ class "flex justify-center gap-1 py-1" ]
        (row
            |> List.map (viewKey shiftOn)
        )


viewKey : Bool -> Key -> Html Msg
viewKey shiftOn key =
    let
        isPressed =
            case key.status of
                Released ->
                    "bg-gray-600"

                Pressed ->
                    "bg-lime-500"

        keyWidth =
            case key.form of
                Normal ->
                    "w-12"

                Special f ->
                    f.extraStyle

        keyView =
            if shiftOn then
                key.altView

            else
                key.view
    in
    div
        [ class <|
            String.join " " [ "relative z-10 px-4 py-2 text-white text-center rounded shadow font-semibold", keyWidth, isPressed ]
        ]
        [ text keyView
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
            [ onKeyDown <| Decode.map KeyDown keyDecoder
            , onKeyUp <| Decode.map KeyUp keyDecoder
            , Time.every 1000 Tick
            ]

    else
        Sub.none


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.map (\a -> a) <|
        Decode.field "code" Decode.string


init : Encode.Value -> ( Model, Cmd Msg )
init flags =
    let
        keyboard =
            Keyboard False False (layoutToList Layout.silPowerG)

        info =
            case Decode.decodeValue infoDecoder flags of
                Ok m ->
                    m

                Err _ ->
                    Info initMetric 0

        model =
            Model keyboard (stringToDictation "") info 0

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
