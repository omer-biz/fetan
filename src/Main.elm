port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict exposing (Dict, update)
import Dictation as DictGen
import Html exposing (Html, a, div, main_, option, p, select, span, table, tbody, td, text, tr)
import Svg exposing (svg, path)
import Svg.Attributes as SvgAttr
import Html.Attributes exposing (class, href, selected, tabindex, target, value)
import Html.Events exposing (onBlur, onFocus, onInput, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode as Encode exposing (dict)
import Models.Layout as Layout exposing (Layout(..))
import Random
import Time
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


type Theme = Light | Dark

type alias Model =
    { keyboard : Keyboard
    , dictation : Dictation
    , info : Info

    -- seconds since last dictation generated
    , time : Float

    , lastKeyEvent : Float
    , currentLayout : Layout
    , layoutKind : Layout.LayoutKind
    , started : Bool
    , theme : Theme
    }


type alias Info =
    { metrics : Metrics
    , lessonIdx : Int
    , layoutKind : String
    }


type alias Metrics =
    { speed : { old : Int, new : Int }
    , accuracy : { old : Int, new : Int }
    , score : { old : Int, new : Int }
    }


type alias Dictation =
    { prev : List Letter
    , current : Maybe Letter
    , next : List Letter
    }


type LetterState
    = Fresh
    | Incorrect
    | Rolling


type alias Letter =
    { letter : Char
    , state : LetterState
    , wasWrong : Bool -- redundent
    }


type alias Keyboard =
    { focusKeyBr : Bool
    , modifier : KeyModifier
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
    | SelectLayout Layout.LayoutKind
    | ToggleTheme


port saveInfo : Encode.Value -> Cmd msg


wordCount : number
wordCount =
    10


dictGenerators : Array (DictGen.Nonempty Char)
dictGenerators =
    Array.fromList [ DictGen.consonantOne, DictGen.consonantTwo, DictGen.consonantThree, DictGen.consonantFour, DictGen.all ]


stringToDictation : String -> Dictation
stringToDictation str =
    case String.uncons str of
        Just ( curr, next ) ->
            Dictation [] (Just (Letter curr Fresh False)) (lettersFromString next)

        Nothing ->
            Dictation [] Nothing []


lettersFromString : String -> List Letter
lettersFromString str =
    str
        |> String.toList
        |> List.map (\l -> Letter l Fresh False)


specialKeys : Dict String String
specialKeys =
    Dict.fromList
        [ ( "Tab", "flex-grow" )
        , ( "CapsLock", "w-20" )
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
            model.currentLayout

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
            ( { model | keyboard = { keyboard | keys = updateKey key Pressed }, lastKeyEvent = 0, started = True }, Cmd.none )

        KeyUp key ->
            let
                ( dict, layout ) =
                    updateDictation key keyboard.modifier model.currentLayout dictation

                nextLessonIdx =
                    if
                        (dict.current == Nothing)
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
                | keyboard = { keyboard | keys = updateKey key Released }
                , currentLayout = layout
                , dictation = dict
                , info = { info | lessonIdx = nextLessonIdx }
              }
            , if dict.current == Nothing then
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
                currList =
                    case dictation.current of
                        Just c -> [ c ]
                        Nothing -> []
                allChars =
                    List.concat [ dictation.prev, currList, dictation.next ]

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
                , lastKeyEvent = 0
                , started = False
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
                    case dictation.current of
                        Just curr ->
                            Layout.hint curr.letter curLayout |> hintToList
                        Nothing ->
                            []

                hintedToList =
                    keyboard.keys
                        |> List.map (hintMod hints)

                keys =
                    if model.lastKeyEvent > 0 then
                        hintedToList

                    else
                        keyboard.keys
            in
            ( { model
                | time = if model.started then model.time + 1 else 0
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
                    -- every time the user presses the mod keys the key rendering function gets called 47
                    -- times it's either this or storing the key views for plain, Shift, CapsLock, and ShiftCapslock
                    -- the classic tradeoff "storage or cpu" or hear me out here, I'm stupid. We will see.
                    -- I just hope the layout authors will not write heavy "renderer"
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    if k.code == key then
                                        { k | state = Pressed }

                                    else
                                        k

                                else
                                    { k | view = Layout.render newState k.code curLayout }
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
                                    { k | view = Layout.render newState k.code curLayout }
                            )
            in
            ( { model
                | keyboard = { keyboard | modifier = newState, keys = keys }
              }
            , Cmd.none
            )

        SelectLayout kind ->
            let
                newLayout =
                    Layout.initLayout kind

                keys =
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    k

                                else
                                    { k | view = Layout.render keyboard.modifier k.code newLayout }
                            )
                newInfo = { info | layoutKind = layoutKindToString kind }
            in
            ( { model
                | layoutKind = kind
                , currentLayout = newLayout
                , keyboard = { keyboard | keys = keys }
                , info = newInfo
              }
            , saveInfo <| encodeInfo newInfo
            )

        ToggleTheme ->
            let
                newTheme =
                    if model.theme == Dark then Light else Dark
            in
            ( { model | theme = newTheme }, saveTheme (if newTheme == Dark then "dark" else "light") )

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


hintToList : Maybe ( KeyModifier, String ) -> List String
hintToList hint =
    let
        modToCode mod code =
            case mod of
                NoModifier ->
                    []

                CapsLock ->
                    [ "CapsLock" ]

                Shift ->
                    [ bestShiftForKey code ]

                ShiftCapsLock ->
                    [ "ShiftRight", "CapsLock" ]
    in
    case hint of
        Just ( mod, code ) ->
            code :: modToCode mod code

        Nothing ->
            []


updateDictation :
    String
    -> KeyModifier
    -> Layout
    -> Dictation
    -> ( Dictation, Layout )
updateDictation codePoint keybrState layout dictation =
    case dictation.current of
        Just current ->
            let
                advanceDictation =
                    case dictation.next of
                        newCurr :: next ->
                            { dictation
                                | next = next
                                , current = Just newCurr
                                , prev = current :: dictation.prev
                            }

                        [] ->
                            { dictation | current = Nothing, prev = current :: dictation.prev }

                wrongAttempt =
                    { dictation | current = Just { current | state = Incorrect, wasWrong = True } }

                rollingCurrent =
                    { dictation | current = Just { current | state = Rolling } }
            in
            case Layout.update keybrState codePoint current.letter layout of
                ( newLayout, Partial ) ->
                    ( rollingCurrent, newLayout )

                ( newLayout, Correct ) ->
                    ( advanceDictation, newLayout )

                ( newLayout, Wrong ) ->
                    ( wrongAttempt, newLayout )

        Nothing ->
            ( dictation, layout )


updateSpeed : Float -> Int -> Metrics -> Metrics
updateSpeed time lenChars metrics =
    let
        speed =
            { old = metrics.speed.new, new = round <| (toFloat lenChars / 5) / (time / 60) }
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


sunIcon : Html msg
sunIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24", SvgAttr.fill "currentColor", SvgAttr.class "w-6 h-6" ]
        [ path
            [ SvgAttr.d "M12 2.25a.75.75 0 01.75.75v2.25a.75.75 0 01-1.5 0V3a.75.75 0 01.75-.75zM7.5 12a4.5 4.5 0 119 0 4.5 4.5 0 01-9 0zM18.894 6.166a.75.75 0 00-1.06-1.06l-1.591 1.59a.75.75 0 101.06 1.061l1.591-1.59zM21.75 12a.75.75 0 01-.75.75h-2.25a.75.75 0 010-1.5H21a.75.75 0 01.75.75zM17.834 18.894a.75.75 0 001.06-1.06l-1.59-1.591a.75.75 0 10-1.061 1.06l1.59 1.591zM12 18a.75.75 0 01.75.75V21a.75.75 0 01-1.5 0v-2.25A.75.75 0 0112 18zM7.758 17.303a.75.75 0 00-1.061-1.06l-1.591 1.59a.75.75 0 001.06 1.061l1.591-1.59zM6 12a.75.75 0 01-.75.75H3a.75.75 0 010-1.5h2.25A.75.75 0 016 12zM6.697 7.757a.75.75 0 001.06-1.06l-1.59-1.591a.75.75 0 00-1.061 1.06l1.59 1.591z"
            ]
            []
        ]

moonIcon : Html msg
moonIcon =
    svg
        [ SvgAttr.viewBox "0 0 24 24", SvgAttr.fill "currentColor", SvgAttr.class "w-6 h-6" ]
        [ path
            [ SvgAttr.fillRule "evenodd", SvgAttr.clipRule "evenodd"
            , SvgAttr.d "M9.528 1.718a.75.75 0 01.162.819A8.97 8.97 0 009 6a9 9 0 009 9 8.97 8.97 0 003.463-.69.75.75 0 01.981.98 10.503 10.503 0 01-9.694 6.46c-5.799 0-10.5-4.701-10.5-10.5 0-4.368 2.667-8.112 6.46-9.694a.75.75 0 01.818.162z"
            ]
            []
        ]

viewThemeToggle : Theme -> Html Msg
viewThemeToggle theme =
    let
        icon =
            if theme == Dark then
                sunIcon
            else
                moonIcon
    in
    Html.button
        [ Html.Events.onClick ToggleTheme
        , class "absolute top-6 right-6 text-stone-600 dark:text-stone-400 opacity-70 hover:opacity-100 transition-opacity"
        ]
        [ icon ]

view : Model -> Html Msg
view model =
    main_ [ class "text-stone-800 dark:text-stone-200 flex items-center justify-center h-screen flex-col transition-colors duration-300 relative" ]
        [ viewThemeToggle model.theme
        , div []
            [ viewInfo model.info
            , viewDictation model.dictation
            , viewKeyBoard model.keyboard
            , viewLayoutSelector model.layoutKind
            ]
        ]


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

layoutKindToString : Layout.LayoutKind -> String
layoutKindToString kind =
    case kind of
        Layout.SilPowerG -> "SilPowerG"
        Layout.PowerGeez -> "PowerGeez"
        Layout.GeezIME -> "GeezIME"


viewLayoutSelector : Layout.LayoutKind -> Html Msg
viewLayoutSelector currentKind =
    let
        ( description, url ) =
            layoutInfo currentKind
    in
    div [ class "flex items-center gap-3 mb-4 w-full pt-4 justify-end" ]
        [ div [ class "relative group inline-block hover:text-gray-100 transition" ]
            [ span
                [ class "relative font-medium text-zinc-400 cursor-help hover:text-gray-100 transition pr-4" ]
                [ text "Layout"
                , span
                    [ class "absolute -top-1 -right-1 text-[10px] text-zinc-500 group-hover:text-zinc-300" ]
                    [ text "ⓘ" ]
                ]
            , span
                [ class "absolute left-0 mt-2 w-64 opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all duration-300 z-50 px-4 py-3 bg-stone-100 dark:bg-stone-900 text-stone-800 dark:text-stone-200 text-sm leading-relaxed border border-stone-300 dark:border-stone-700 shadow-xl rounded-sm md:block" ]
                [ div [ class "mb-2" ] [ text description ]
                , a
                    [ href url
                    , target "_blank"
                    , class "text-emerald-400 hover:text-emerald-300 underline"
                    ]
                    [ text "View full layout table →" ]
                ]
            ]
        , select
            [ onInput (layoutKindFromString >> SelectLayout)
            , class
                "bg-stone-100 dark:bg-stone-800 text-stone-800 dark:text-stone-200 text-sm border border-stone-300 dark:border-stone-700 rounded-md px-3 py-1.5 focus:outline-none focus:ring-2 focus:ring-teal-500 hover:bg-stone-200 dark:hover:bg-stone-700 transition"
            ]
            [ option
                [ value "SilPowerG"
                , selected (currentKind == Layout.SilPowerG)
                ]
                [ text "SilPowerG" ]
            , option
                [ value "PowerGeez"
                , selected (currentKind == Layout.PowerGeez)
                ]
                [ text "PowerGeez" ]
            , option
                [ value "GeezIME"
                , selected (currentKind == Layout.GeezIME)
                ]
                [ text "GeezIME" ]
            ]
        ]


layoutInfo : Layout.LayoutKind -> ( String, String )
layoutInfo kind =
    case kind of
        Layout.SilPowerG ->
            ( "SIL Power-G is a phonetic Amharic keyboard layout widely used in Ethiopia. It maps Latin keys to Ethiopic letters based on sound."
            , "https://help.keyman.com/keyboard/sil_ethiopic_power_g/1.2.6/sil_ethiopic_power_g"
            )

        Layout.PowerGeez ->
            ( "PowerGeez is a legacy Ethiopian typing system used in many older applications and publishing tools."
            , "/layouts/powergeez"
            )

        Layout.GeezIME ->
            ( "GeezIME is a transliteration input method. You type Latin sequences like 'he', 'hu', 'hi' which convert into Ethiopic characters."
            , "https://geezlab.com/help/"
            )


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
    dictGenerators
        |> Array.get idx
        |> Maybe.withDefault DictGen.consonantOne
        |> DictGen.toList
        |> List.map (\k -> span [ class "px-1" ] [ text <| String.fromChar k ])
        |> td []


viewMetrics : Metrics -> Html msg
viewMetrics metrics =
    let
        viewOld m pst =
            if m.new >= m.old then
                span [ class "text-green-600 dark:text-green-400" ] [ text <| "+" ++ String.fromInt (m.new - m.old) ++ pst ]

            else
                span [ class "text-red-600 dark:text-red-400" ] [ text <| String.fromInt (m.new - m.old) ++ pst ]

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
            case dict.current of
                Just current ->
                    if
                        (current.wasWrong && (current.state /= Rolling))
                            || current.state
                            == Incorrect
                    then
                        "text-red-600 dark:text-red-400"

                    else if current.state == Rolling then
                        "text-amber-600 dark:text-amber-400"

                    else
                        "text-teal-600 dark:text-teal-400"
                Nothing ->
                    ""

        viewLetter lt =
            let
                wasWrong =
                    if lt.wasWrong then
                        "text-red-600 dark:text-red-400"

                    else
                        ""
            in
            span [ class wasWrong ]
                [ lt.letter
                    |> String.fromChar
                    |> String.replace " " " · "
                    |> text
                ]

        viewCurrentLetter =
            case dict.current of
                Just current ->
                    span
                        [ class "border-teal-600 dark:border-teal-400 border-b-4" ]
                        [ current.letter
                            |> String.fromChar
                            |> String.replace " " " · "
                            |> text
                        ]
                Nothing ->
                    text ""

        viewLetters lts =
            List.map viewLetter lts
    in
    div [ class "mx-auto border rounded border-2 border-stone-300 dark:border-stone-700 p-4 mb-4 max-w-[800px] text-3xl font-normal leading-relaxed transition-colors duration-300" ]
        [ p [ class "inline m-0 p-0 text-stone-400 dark:text-stone-500" ] (viewLetters (List.reverse dict.prev))
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
                div [ class "absolute z-20 inset-0 bg-opacity-5 backdrop-blur-sm flex items-center justify-center cursor-pointer" ]
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
        [ class <| "border-2 p-6 rounded border-stone-300 dark:border-stone-800 bg-stone-100 dark:bg-stone-900/50 relative transition-colors duration-300"
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
                    "bg-teal-500 text-stone-50 dark:text-stone-950"

                Released ->
                    "bg-stone-200 dark:bg-stone-800 text-stone-700 dark:text-stone-300 border border-stone-300 dark:border-stone-700"

                Hinted ->
                    "transition-colors duration-300 bg-teal-100 dark:bg-teal-900/50 text-teal-800 dark:text-teal-200 border border-teal-400 dark:border-teal-700 animate-pulse"

        extraStyle =
            Dict.get key.code specialKeys
                |> Maybe.withDefault ""
    in
    div
        [ class <| String.join " " [ "relative z-10 x-4 py-2 text-center rounded shadow font-semibold w-12", bg, extraStyle ] ]
        [ text key.view
        , case String.split "Key" key.code of
            "" :: "F" :: [] ->
                span [ class "absolute z-2 bottom-0 inset-x-0 text-2xl" ] [ text "." ]

            "" :: "J" :: [] ->
                span [ class "absolute z-2 bottom-0 inset-x-0 text-2xl" ] [ text "." ]

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
    [ "ShiftLeft", "ShiftRight", "CapsLock", "ControlRight", "ControlLeft", "AltRight", "AltLeft", "Tab", "MetaLeft", "MetaRight", "Enter" ]


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
    Key "⇪" "CapsLock" Released


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
        info =
            case Decode.decodeValue (Decode.field "lessonInfo" infoDecoder) flags of
                Ok m ->
                    m

                Err _ ->
                    case Decode.decodeValue infoDecoder flags of
                        Ok m -> m
                        Err _ -> Info initMetric 0 "GeezIME"

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

        themeStr =
            case Decode.decodeValue (Decode.field "theme" Decode.string) flags of
                Ok "light" -> Light
                _ -> Dark

        model =
            Model keyboard (stringToDictation "") info 0 0 curLayout curLayoutKind False themeStr

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
    Decode.map3 Info
        (Decode.field "metrics" metricsDecoder)
        (Decode.field "lessonIdx" Decode.int)
        (Decode.maybe (Decode.field "layoutKind" Decode.string) |> Decode.map (Maybe.withDefault "GeezIME"))


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
        , ( "layoutKind", Encode.string info.layoutKind )
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


port saveTheme : String -> Cmd msg
