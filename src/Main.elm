port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict exposing (Dict, update)
import Dictation as DictGen
import Html exposing (Html, a, div, main_, option, p, select, span, table, tbody, td, text, tr)
import Html.Attributes exposing (class, href, selected, tabindex, target, value)
import Html.Events exposing (onBlur, onFocus, onInput, preventDefaultOn)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Json.Encode as Encode exposing (dict)
import Models.Layout as Layout exposing (Layout(..))
import Random
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Time
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


type Theme
    = Light
    | Dark


type alias Model =
    { keyboard : Keyboard
    , dictation : Dictation
    , info : Info

    -- seconds since last dictation generated
    , time : Float
    , lastSuccessTime : Float
    , lastKeyEvent : Float
    , currentLayout : Layout
    , layoutKind : Layout.LayoutKind
    , started : Bool
    , theme : Theme
    }


type alias LetterStat =
    { errorEma : Float
    , latencyEma : Float
    , count : Int
    }


type alias Info =
    { metrics : Metrics
    , lessonIdx : Int
    , layoutKind : String
    , dictationsCompleted : Int
    , letterStats : Dict.Dict String LetterStat
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


type AttemptResult
    = WasCorrect
    | WasWrong
    | WasPartial
    | NoOpResult


type alias KeyEvent =
    { code : String
    , timeStamp : Float
    }


type Msg
    = NoOp
    | KeyDown KeyEvent
    | KeyUp KeyEvent
    | ModKeyDown String
    | ModKeyUp String
    | FocusKeyBr
    | BlurKeyBr
    | NewDict String
    | Tick Time.Posix
    | SelectLayout Layout.LayoutKind
    | ToggleTheme


port saveInfo : Encode.Value -> Cmd msg


learningSequence : Array.Array String
learningSequence =
    Array.fromList [ "ሀ", "ለ", "በ", "መ", "ነ", "ረ", "ሰ", "ከ", "ቀ", "ወ", "ተ", "ቸ", "ዘ", "ደ", "ጀ", "አ", "ፈ", "ፐ", "ሐ", "ዐ", "ኀ", "ሸ", "የ", "ሠ", "ኘ", "ገ", "ጠ", "ጨ", "ጰ", "ጸ", "ፀ", "ዠ", "ኸ" ]


getBaseLetterForLesson : Int -> String
getBaseLetterForLesson idx =
    Array.get (idx - 1) learningSequence |> Maybe.withDefault "ሀ"


wordCount : number
wordCount =
    10


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

        KeyDown keyEvent ->
            let
                newLastSuccessTime =
                    if not model.started then
                        keyEvent.timeStamp

                    else
                        model.lastSuccessTime
            in
            ( { model | keyboard = { keyboard | keys = updateKey keyEvent.code Pressed }, lastKeyEvent = 0, started = True, lastSuccessTime = newLastSuccessTime }, Cmd.none )

        KeyUp keyEvent ->
            let
                ( dict, layout, attemptResult ) =
                    updateDictation keyEvent.code keyboard.modifier model.currentLayout dictation

                updates =
                    let
                        targetLetter =
                            Maybe.map (\curr -> String.fromChar curr.letter) dictation.current |> Maybe.withDefault ""

                        oldStat =
                            Dict.get targetLetter info.letterStats |> Maybe.withDefault { errorEma = 0, latencyEma = 0, count = 0 }

                        ( updatedStats, updatedTime ) =
                            case attemptResult of
                                NoOpResult ->
                                    ( info.letterStats, model.lastSuccessTime )

                                WasPartial ->
                                    ( info.letterStats, model.lastSuccessTime )

                                WasWrong ->
                                    let
                                        newStat =
                                            { oldStat | errorEma = 0.1 * 1.0 + 0.9 * oldStat.errorEma }
                                    in
                                    ( Dict.insert targetLetter newStat info.letterStats, model.lastSuccessTime )

                                WasCorrect ->
                                    let
                                        latency =
                                            keyEvent.timeStamp - model.lastSuccessTime

                                        newStat =
                                            if oldStat.count == 0 then
                                                { oldStat | count = 1, latencyEma = latency, errorEma = 0 }

                                            else
                                                { oldStat
                                                    | count = oldStat.count + 1
                                                    , latencyEma = 0.1 * latency + 0.9 * oldStat.latencyEma
                                                    , errorEma = 0.9 * oldStat.errorEma
                                                }
                                    in
                                    ( Dict.insert targetLetter newStat info.letterStats, keyEvent.timeStamp )

                        isFinished =
                            dict.current == Nothing

                        ( finalLessonIdx, finalCompleted ) =
                            if isFinished then
                                let
                                    baseLetter =
                                        getBaseLetterForLesson info.lessonIdx

                                    stat =
                                        Dict.get baseLetter updatedStats |> Maybe.withDefault { errorEma = 0, latencyEma = 0, count = 0 }

                                    accuracyScore =
                                        clamp 0 1 (1.0 - (stat.errorEma * 10))

                                    speedScore =
                                        clamp 0 1 ((2500 - stat.latencyEma) / 1300)

                                    conf =
                                        0.5 + (accuracyScore * 0.4) + (speedScore * 0.1)
                                in
                                if stat.count >= 15 && conf > 0.85 && info.lessonIdx < 34 then
                                    ( info.lessonIdx + 1, 0 )

                                else
                                    ( info.lessonIdx, info.dictationsCompleted + 1 )

                            else
                                ( info.lessonIdx, info.dictationsCompleted )
                    in
                    { nextLessonIdx = finalLessonIdx
                    , nextCompleted = finalCompleted
                    , nextStats = updatedStats
                    , newLastSuccessTime = updatedTime
                    }
            in
            ( { model
                | keyboard = { keyboard | keys = updateKey keyEvent.code Released }
                , currentLayout = layout
                , dictation = dict
                , info = { info | lessonIdx = updates.nextLessonIdx, dictationsCompleted = updates.nextCompleted, letterStats = updates.nextStats }
                , lastSuccessTime = updates.newLastSuccessTime
              }
            , if dict.current == Nothing then
                DictGen.genForLevel updates.nextLessonIdx
                    |> Random.generate NewDict

              else
                Cmd.none
            )

        NewDict dict ->
            let
                currList =
                    case dictation.current of
                        Just c ->
                            [ c ]

                        Nothing ->
                            []

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
                | time =
                    if model.started then
                        model.time + 1

                    else
                        0
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

                newInfo =
                    { info | layoutKind = layoutKindToString kind }
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
                    if model.theme == Dark then
                        Light

                    else
                        Dark
            in
            ( { model | theme = newTheme }
            , saveTheme
                (if newTheme == Dark then
                    "dark"

                 else
                    "light"
                )
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
    -> ( Dictation, Layout, AttemptResult )
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
                    ( rollingCurrent, newLayout, WasPartial )

                ( newLayout, Correct ) ->
                    ( advanceDictation, newLayout, WasCorrect )

                ( newLayout, Wrong ) ->
                    ( wrongAttempt, newLayout, WasWrong )

        Nothing ->
            ( dictation, layout, NoOpResult )


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
            [ SvgAttr.fillRule "evenodd"
            , SvgAttr.clipRule "evenodd"
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
        , Html.Attributes.id "theme-toggle"
        , class "absolute top-6 right-6 text-stone-600 dark:text-stone-400 opacity-70 hover:opacity-100 transition-opacity"
        ]
        [ icon ]


view : Model -> Html Msg
view model =
    main_ [ class "text-stone-800 dark:text-stone-200 flex flex-col items-center justify-center min-h-screen relative px-4 sm:px-8 py-12" ]
        [ viewThemeToggle model.theme
        , div [ class "w-full max-w-[800px] flex flex-col items-center" ]
            [ viewInfo model.info
            , viewDictation model.dictation
            , viewKeyBoard model.keyboard
            , viewLayoutSelector model.layoutKind
            ]
        , Html.footer [ class "absolute bottom-4 text-sm text-stone-500 dark:text-stone-400 flex gap-1" ]
            [ text "an open-source project | made by "
            , a
                [ href "https://github.com/omer-biz/fetan"
                , target "_blank"
                , class "font-medium hover:text-teal-600 dark:hover:text-teal-400 transition-colors"
                ]
                [ text "omer" ]
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
        Layout.SilPowerG ->
            "SilPowerG"

        Layout.PowerGeez ->
            "PowerGeez"

        Layout.GeezIME ->
            "GeezIME"


viewLayoutSelector : Layout.LayoutKind -> Html Msg
viewLayoutSelector currentKind =
    let
        ( description, url ) =
            layoutInfo currentKind
    in
    div [ class "flex items-center gap-3 mb-4 w-full pt-8 md:pt-4 justify-center md:justify-end" ]
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
    div [ class "flex flex-col items-center mb-8 w-full max-w-[800px]" ]
        [ viewMetrics info
        , div [ class "mt-4 w-full flex justify-center" ]
            [ viewProgression info.lessonIdx
            ]
        ]


viewProgression : Int -> Html msg
viewProgression idx =
    let
        effIdx =
            clamp 1 33 idx
    in
    div [ class "flex flex-wrap gap-2 md:gap-3 justify-center items-baseline text-sm md:text-base select-none mt-2" ]
        (List.indexedMap
            (\i c ->
                let
                    letterIdx = i + 1
                    
                    stateClasses =
                        if letterIdx < effIdx then
                            "text-stone-800 dark:text-stone-200 font-medium"
                        else if letterIdx == effIdx then
                            "text-teal-600 dark:text-teal-400 font-bold border-b-2 border-teal-500/50 pb-0.5"
                        else
                            "text-stone-400 dark:text-stone-500 font-normal opacity-80"
                in
                span [ class ("transition-colors duration-300 " ++ stateClasses) ]
                    [ text (String.fromChar c) ]
            )
            DictGen.learningSequence
        )





viewMetrics : Info -> Html msg
viewMetrics info =
    let
        metrics =
            info.metrics

        baseLetter =
            getBaseLetterForLesson info.lessonIdx

        stat =
            Dict.get baseLetter info.letterStats |> Maybe.withDefault { errorEma = 0, latencyEma = 0, count = 0 }

        accuracyScore =
            clamp 0 1 (1.0 - (stat.errorEma * 10))

        speedScore =
            clamp 0 1 ((2500 - stat.latencyEma) / 1300)

        conf =
            if stat.count < 15 then
                0.84 * (toFloat stat.count / 15.0)

            else
                0.5 + (accuracyScore * 0.4) + (speedScore * 0.1)

        confStr =
            String.fromInt (round (conf * 100))

        viewMetric label m pst =
            div [ class "flex flex-col items-center p-3 md:p-4 bg-white dark:bg-stone-800/80 rounded-xl shadow-sm border border-stone-200 dark:border-stone-700/50 flex-1 min-w-[100px] md:min-w-[120px]" ]
                [ span [ class "text-[10px] md:text-[11px] text-stone-500 dark:text-stone-400 uppercase tracking-widest mb-1 font-semibold text-center" ] [ text label ]
                , div [ class "flex items-baseline gap-1" ]
                    [ span [ class "text-2xl md:text-3xl font-light text-stone-800 dark:text-stone-100" ] [ text m ]
                    , span [ class "text-xs md:text-sm font-medium text-stone-400 dark:text-stone-500" ] [ text pst ]
                    ]
                ]
    in
    div [ class "flex flex-wrap justify-center gap-3 md:gap-6 w-full" ]
        [ viewMetric "Speed" (String.fromInt metrics.speed.new) "wpm"
        , viewMetric "Accuracy" (String.fromInt metrics.accuracy.new) "%"
        , viewMetric "Confidence" confStr "%"
        , viewMetric "Score" (String.fromInt metrics.score.new) ""
        ]


viewDictation : Dictation -> Html msg
viewDictation dict =
    let
        currentIndex =
            List.length dict.prev

        allLetters =
            List.reverse dict.prev
                ++ (case dict.current of
                        Just c ->
                            [ c ]

                        Nothing ->
                            []
                   )
                ++ dict.next

        viewLetter idx lt =
            let
                isCurrent =
                    idx == currentIndex

                isSpace =
                    lt.letter == ' '

                spaceClass =
                    if isSpace then
                        " px-[0.15em] text-center"

                    else
                        ""

                colorClass =
                    if isCurrent then
                        if (lt.wasWrong && (lt.state /= Rolling)) || lt.state == Incorrect then
                            "bg-red-500/20 dark:bg-red-500/30 text-red-600 dark:text-red-400"

                        else if lt.state == Rolling then
                            "bg-amber-500/20 dark:bg-amber-500/30 text-amber-600 dark:text-amber-400"

                        else
                            "text-teal-600 dark:text-teal-400 relative z-10"

                    else if idx < currentIndex then
                        if lt.wasWrong then
                            "text-red-600 dark:text-red-400 opacity-60"

                        else
                            "text-stone-300 dark:text-stone-600"

                    else
                        "text-stone-800 dark:text-stone-200"

                classes =
                    String.join " " [ "relative rounded-sm py-0.5", spaceClass, colorClass ]

            in
            ( String.fromInt idx
            , span [ class classes, if isCurrent then Html.Attributes.id "active-letter" else class "" ]
                [ if isSpace then
                    text "·\u{200B}"

                  else
                    text (String.fromChar lt.letter)
                ]
            )
    in
    Keyed.node "div"
        [ class "mx-auto bg-white dark:bg-stone-900/40 border rounded-2xl border-stone-200 dark:border-stone-800 p-4 sm:p-6 md:p-8 mb-6 md:mb-8 w-full text-2xl sm:text-3xl md:text-4xl font-normal leading-loose tracking-wide shadow-sm " ]
        (List.indexedMap viewLetter allLetters)


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
    div [ class "w-full overflow-hidden flex justify-center pb-8 -mb-8" ]
        [ div
            [ class <| "border-2 p-3 sm:p-4 md:p-6 rounded-xl border-stone-300 dark:border-stone-800 bg-stone-100 dark:bg-stone-900/50 relative transition-all duration-300 transform origin-top scale-[0.45] sm:scale-[0.65] md:scale-[0.85] lg:scale-100"
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
        ]


viewRow : List (Html msg) -> Html msg
viewRow row =
    div [ class "flex gap-1 py-1" ] row


fingerColorClass : String -> String
fingerColorClass code =
    case code of
        "KeyQ" ->
            "border-b-[4px] border-b-pink-400 dark:border-b-pink-600/50"

        "KeyA" ->
            "border-b-[4px] border-b-pink-400 dark:border-b-pink-600/50"

        "KeyZ" ->
            "border-b-[4px] border-b-pink-400 dark:border-b-pink-600/50"

        "ShiftLeft" ->
            "border-b-[4px] border-b-pink-400 dark:border-b-pink-600/50"

        "Tab" ->
            "border-b-[4px] border-b-pink-400 dark:border-b-pink-600/50"

        "CapsLock" ->
            "border-b-[4px] border-b-pink-400 dark:border-b-pink-600/50"

        "KeyW" ->
            "border-b-[4px] border-b-orange-400 dark:border-b-orange-600/50"

        "KeyS" ->
            "border-b-[4px] border-b-orange-400 dark:border-b-orange-600/50"

        "KeyX" ->
            "border-b-[4px] border-b-orange-400 dark:border-b-orange-600/50"

        "KeyE" ->
            "border-b-[4px] border-b-yellow-400 dark:border-b-yellow-600/50"

        "KeyD" ->
            "border-b-[4px] border-b-yellow-400 dark:border-b-yellow-600/50"

        "KeyC" ->
            "border-b-[4px] border-b-yellow-400 dark:border-b-yellow-600/50"

        "KeyR" ->
            "border-b-[4px] border-b-green-400 dark:border-b-green-600/50"

        "KeyF" ->
            "border-b-[4px] border-b-green-400 dark:border-b-green-600/50"

        "KeyV" ->
            "border-b-[4px] border-b-green-400 dark:border-b-green-600/50"

        "KeyT" ->
            "border-b-[4px] border-b-green-400 dark:border-b-green-600/50"

        "KeyG" ->
            "border-b-[4px] border-b-green-400 dark:border-b-green-600/50"

        "KeyB" ->
            "border-b-[4px] border-b-green-400 dark:border-b-green-600/50"

        "KeyY" ->
            "border-b-[4px] border-b-cyan-400 dark:border-b-cyan-600/50"

        "KeyH" ->
            "border-b-[4px] border-b-cyan-400 dark:border-b-cyan-600/50"

        "KeyN" ->
            "border-b-[4px] border-b-cyan-400 dark:border-b-cyan-600/50"

        "KeyU" ->
            "border-b-[4px] border-b-cyan-400 dark:border-b-cyan-600/50"

        "KeyJ" ->
            "border-b-[4px] border-b-cyan-400 dark:border-b-cyan-600/50"

        "KeyM" ->
            "border-b-[4px] border-b-cyan-400 dark:border-b-cyan-600/50"

        "KeyI" ->
            "border-b-[4px] border-b-blue-400 dark:border-b-blue-600/50"

        "KeyK" ->
            "border-b-[4px] border-b-blue-400 dark:border-b-blue-600/50"

        "Comma" ->
            "border-b-[4px] border-b-blue-400 dark:border-b-blue-600/50"

        "KeyO" ->
            "border-b-[4px] border-b-indigo-400 dark:border-b-indigo-600/50"

        "KeyL" ->
            "border-b-[4px] border-b-indigo-400 dark:border-b-indigo-600/50"

        "Period" ->
            "border-b-[4px] border-b-indigo-400 dark:border-b-indigo-600/50"

        "KeyP" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "Semicolon" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "Slash" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "BracketLeft" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "BracketRight" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "Quote" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "Backslash" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "ShiftRight" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "Enter" ->
            "border-b-[4px] border-b-purple-400 dark:border-b-purple-600/50"

        "Space" ->
            "border-b-[4px] border-b-stone-300 dark:border-b-stone-600/50"

        "AltLeft" ->
            "border-b-[4px] border-b-stone-300 dark:border-b-stone-600/50"

        "AltRight" ->
            "border-b-[4px] border-b-stone-300 dark:border-b-stone-600/50"

        "ControlLeft" ->
            "border-b-[4px] border-b-stone-300 dark:border-b-stone-600/50"

        "ControlRight" ->
            "border-b-[4px] border-b-stone-300 dark:border-b-stone-600/50"

        _ ->
            "border-b-[4px] border-b-stone-300 dark:border-b-stone-600/50"


viewKey : Key -> Html msg
viewKey key =
    let
        bg =
            case key.state of
                Pressed ->
                    "bg-teal-500 text-stone-50 dark:text-stone-950 border-b-0 translate-y-[4px]"

                Released ->
                    "bg-stone-100 dark:bg-stone-800 text-stone-700 dark:text-stone-300 border-t border-l border-r border-stone-200 dark:border-stone-700 " ++ fingerColorClass key.code

                Hinted ->
                    "bg-teal-100 dark:bg-teal-900/70 text-teal-900 dark:text-teal-100 border-2 border-teal-400 dark:border-teal-400 shadow-[0_0_10px_rgba(20,184,166,0.5)] animate-pulse"

        extraStyle =
            Dict.get key.code specialKeys
                |> Maybe.withDefault ""
    in
    div
        [ class <| String.join " " [ "relative z-10 x-4 py-2 text-center rounded-lg shadow-sm font-semibold w-12 transition-transform duration-75", bg, extraStyle ] ]
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


keyDecoder : Decode.Decoder KeyEvent
keyDecoder =
    Decode.map2 KeyEvent
        (Decode.field "code" Decode.string)
        (Decode.field "timeStamp" Decode.float)


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
                        Ok m ->
                            m

                        Err _ ->
                            Info initMetric 1 "GeezIME" 0 Dict.empty

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
                Ok "light" ->
                    Light

                _ ->
                    Dark

        model =
            { keyboard = keyboard
            , dictation = stringToDictation ""
            , info = info
            , time = 0
            , lastSuccessTime = 0
            , lastKeyEvent = 0
            , currentLayout = curLayout
            , layoutKind = curLayoutKind
            , started = False
            , theme = themeStr
            }

        dictation =
            DictGen.genForLevel info.lessonIdx
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


letterStatDecoder : Decode.Decoder LetterStat
letterStatDecoder =
    Decode.map3 LetterStat
        (Decode.field "errorEma" Decode.float)
        (Decode.field "latencyEma" Decode.float)
        (Decode.field "count" Decode.int)


statsDictDecoder : Decode.Decoder (Dict.Dict String LetterStat)
statsDictDecoder =
    Decode.dict letterStatDecoder


infoDecoder : Decode.Decoder Info
infoDecoder =
    Decode.map5 Info
        (Decode.field "metrics" metricsDecoder)
        (Decode.field "lessonIdx" Decode.int)
        (Decode.maybe (Decode.field "layoutKind" Decode.string) |> Decode.map (Maybe.withDefault "GeezIME"))
        (Decode.maybe (Decode.field "dictationsCompleted" Decode.int) |> Decode.map (Maybe.withDefault 0))
        (Decode.maybe (Decode.field "letterStats" statsDictDecoder) |> Decode.map (Maybe.withDefault Dict.empty))


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


encodeLetterStat : LetterStat -> Encode.Value
encodeLetterStat stat =
    Encode.object
        [ ( "errorEma", Encode.float stat.errorEma )
        , ( "latencyEma", Encode.float stat.latencyEma )
        , ( "count", Encode.int stat.count )
        ]


encodeInfo : Info -> Encode.Value
encodeInfo info =
    Encode.object
        [ ( "metrics", encodeMetrics info.metrics )
        , ( "lessonIdx", Encode.int info.lessonIdx )
        , ( "layoutKind", Encode.string info.layoutKind )
        , ( "dictationsCompleted", Encode.int info.dictationsCompleted )
        , ( "letterStats", Encode.dict identity encodeLetterStat info.letterStats )
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
