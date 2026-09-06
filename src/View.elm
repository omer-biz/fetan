module View exposing (view)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Community
import Dict exposing (Dict)
import Dictation as DictGen
import DictationLogic exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Json.Decode as Decode
import Models.Layout as Layout exposing (Layout(..))
import Ports
import Routing exposing (Route(..))
import Stats exposing (LetterStat, SessionRecord)
import Storage
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Time
import Types.Core exposing (..)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))
import Types.Msg exposing (..)

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
        , class "text-stone-600 dark:text-stone-400 opacity-70 hover:opacity-100 transition-opacity flex items-center"
        ]
        [ icon ]


communityIcon : Html msg
communityIcon =
    Svg.svg
        [ SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        , SvgAttr.class "w-5 h-5"
        ]
        [ Svg.path [ SvgAttr.d "M17 21v-2a4 4 0 0 0-4-4H5a4 4 0 0 0-4 4v2" ] []
        , Svg.circle [ SvgAttr.cx "9", SvgAttr.cy "7", SvgAttr.r "4" ] []
        , Svg.path [ SvgAttr.d "M23 21v-2a4 4 0 0 0-3-3.87" ] []
        , Svg.path [ SvgAttr.d "M16 3.13a4 4 0 0 1 0 7.75" ] []
        ]


statsIcon : Html msg
statsIcon =
    Svg.svg
        [ SvgAttr.width "20"
        , SvgAttr.height "20"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.fill "none"
        , SvgAttr.stroke "currentColor"
        , SvgAttr.strokeWidth "2"
        , SvgAttr.strokeLinecap "round"
        , SvgAttr.strokeLinejoin "round"
        ]
        [ Svg.path [ SvgAttr.d "M3 3v18h18" ] []
        , Svg.path [ SvgAttr.d "M18 17V9" ] []
        , Svg.path [ SvgAttr.d "M13 17V5" ] []
        , Svg.path [ SvgAttr.d "M8 17v-3" ] []
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    Html.header [ class "relative z-10 w-full flex justify-between items-center mb-8" ]
        [ div [ class "flex items-center" ]
            [ span [ class "text-xl font-medium tracking-[0.2em] text-slate-700 dark:text-slate-300 lowercase" ] [ text "qelm" ]
            ]
        , div [ class "flex items-center gap-4 md:gap-6" ]
            [ viewLayoutSelector model.layoutKind
            , if model.route == StatsRoute || model.route == CommunityRoute then
                Html.button
                    [ Html.Events.onClick (GoTo TypingRoute)
                    , class "text-stone-600 dark:text-stone-400 opacity-70 hover:opacity-100 transition-opacity flex items-center text-sm font-medium gap-1"
                    ]
                    [ text "Back" ]

              else
                Html.div [ class "flex items-center gap-3" ]
                    [ Html.button
                        [ Html.Events.onClick (GoTo StatsRoute)
                        , class "text-stone-600 dark:text-stone-400 opacity-70 hover:opacity-100 transition-opacity flex items-center"
                        ]
                        [ statsIcon ]
                    , Html.button
                        [ Html.Events.onClick (GoTo CommunityRoute)
                        , class "text-stone-600 dark:text-stone-400 opacity-70 hover:opacity-100 transition-opacity flex items-center"
                        ]
                        [ communityIcon ]
                    ]
            , viewThemeToggle model.theme
            ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "qelm"
    , body =
        [ main_ [ class "bg-stone-200 dark:bg-[#282828] text-stone-800 dark:text-stone-200 flex flex-col items-center min-h-screen relative px-4 sm:px-8 py-6 w-full" ]
            [ div [ class "w-full max-w-[1000px] flex flex-col items-center flex-1" ]
                [ viewHeader model
                , if model.route == StatsRoute then
                    Stats.viewStats { history = model.info.history, letterStats = (getCurrentLayoutData model.info).letterStats, hoveringStats = model.hoveringStats, hoveringMastery = model.hoveringMastery, currentTime = model.currentTime, zone = model.zone, aggregate = model.info.aggregate } OnHoverStats OnHoverMastery

                  else if model.route == CommunityRoute then
                    Html.map CommunityMsg (Community.view model.communityData)

                  else
                    div [ class "w-full max-w-[800px] flex flex-col items-center flex-1 justify-center -mt-16" ]
                        [ viewInfo model.info model.justLeveledUp
                        , viewDictation model.dictation
                        , viewKeyBoard model.keyboard
                        ]
                ]
            , Html.footer [ class "absolute bottom-4 text-sm text-stone-500 dark:text-stone-400 flex gap-1" ]
                [ text "an open-source project | made by "
                , a
                    [ href "https://github.com/omer-biz/qelm"
                    , target "_blank"
                    , class "font-medium hover:text-slate-600 dark:hover:text-slate-400 transition-colors"
                    ]
                    [ text "omer" ]
                ]
            ]
        ]
    }


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
    div [ class "flex items-center gap-2" ]
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
                "bg-stone-100 dark:bg-stone-800 text-stone-800 dark:text-stone-200 text-sm border border-stone-300 dark:border-stone-700 rounded-md px-3 py-1.5 focus:outline-none focus:ring-2 focus:ring-slate-500 hover:bg-stone-200 dark:hover:bg-stone-700 transition"
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


viewInfo : Info -> Bool -> Html Msg
viewInfo info justLeveledUp =
    div [ class "flex flex-col items-center mb-8 w-full max-w-[800px]" ]
        [ viewMetrics info
        , div [ class "mt-4 w-full flex justify-center" ]
            [ viewProgression (getCurrentLayoutData info).lessonIdx justLeveledUp
            ]
        ]


viewProgression : Int -> Bool -> Html msg
viewProgression idx justLeveledUp =
    let
        effIdx =
            clamp 1 33 idx
    in
    div [ class "flex flex-wrap gap-2 md:gap-3 justify-center items-baseline text-sm md:text-base select-none mt-2" ]
        (List.indexedMap
            (\i c ->
                let
                    letterIdx =
                        i + 1

                    stateClasses =
                        if letterIdx < effIdx then
                            "text-stone-800 dark:text-stone-200 font-medium"

                        else if letterIdx == effIdx then
                            if justLeveledUp then
                                "text-emerald-500 dark:text-emerald-400 font-bold border-b-2 border-emerald-500 pb-0.5 animate-bounce scale-125 shadow-emerald-500/50"

                            else
                                "text-slate-600 dark:text-slate-400 font-bold border-b-2 border-slate-500/50 pb-0.5"

                        else
                            "text-stone-400 dark:text-stone-500 tracking-wide font-normal opacity-80"
                in
                span [ id ("progression-letter-" ++ String.fromInt letterIdx), class ("transition-all duration-300 transform " ++ stateClasses) ]
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
            getBaseLetterForLesson (getCurrentLayoutData info).lessonIdx

        stat =
            getFamilyStats baseLetter (getCurrentLayoutData info).letterStats

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

        viewMetric label m pst tooltip =
            div [ class "relative group flex flex-col items-center p-3 md:p-4 bg-white dark:bg-stone-800/80 rounded-lg shadow-[0_2px_12px_rgb(0,0,0,0.03)] dark:shadow-none border border-stone-200/80 dark:border-stone-700/50 flex-1 min-w-[100px] md:min-w-[120px]" ]
                [ div [ class "flex items-center gap-1 mb-1" ]
                    [ span [ class "text-[10px] md:text-[11px] text-stone-500 dark:text-stone-400 uppercase tracking-widest font-semibold text-center" ] [ text label ]
                    , if String.isEmpty tooltip then
                        text ""

                      else
                        div [ class "relative flex items-center justify-center w-3 h-3 rounded-full border border-stone-300 dark:border-stone-600 text-[9px] text-stone-400 dark:text-stone-500 cursor-help" ]
                            [ text "?"
                            , div [ class "absolute bottom-full mb-2 left-1/2 -translate-x-1/2 w-48 p-2 bg-stone-800 dark:bg-stone-200 text-stone-100 dark:text-stone-800 text-[11px] leading-tight rounded shadow-xl opacity-0 invisible group-hover:opacity-100 group-hover:visible transition-all duration-200 z-50 normal-case tracking-normal font-normal text-center pointer-events-none" ]
                                [ text tooltip
                                , div [ class "absolute top-full left-1/2 -translate-x-1/2 border-4 border-transparent border-t-stone-800 dark:border-t-stone-200" ] []
                                ]
                            ]
                    ]
                , div [ class "flex items-baseline gap-1" ]
                    [ span [ class "text-2xl md:text-3xl font-light text-stone-800 dark:text-stone-100" ] [ text m ]
                    , span [ class "text-xs md:text-sm font-medium text-stone-400 dark:text-stone-500 tracking-wide" ] [ text pst ]
                    ]
                ]
    in
    div [ class "flex flex-wrap justify-center gap-3 md:gap-6 w-full" ]
        [ viewMetric "Speed" (String.fromInt metrics.speed.new) "wpm" ""
        , viewMetric "Accuracy" (String.fromInt metrics.accuracy.new) "%" ""
        , viewMetric "Mastery" confStr "%" "Mastery reflects how consistently and quickly you can type this lesson's characters."
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
                            "text-slate-600 dark:text-slate-400 relative z-10"

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
            , span
                [ class classes
                , if isCurrent then
                    Html.Attributes.id "active-letter"

                  else
                    class ""
                ]
                [ if isSpace then
                    text " "

                  else
                    text (String.fromChar lt.letter)
                ]
            )
    in
    Keyed.node "div"
        [ class "whitespace-pre-wrap mx-auto bg-white dark:bg-stone-900/40 border rounded-xl border-stone-200 dark:border-stone-800 p-4 sm:p-6 md:p-8 mb-6 md:mb-8 w-full text-2xl sm:text-3xl md:text-4xl font-normal leading-loose tracking-wide shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none " ]
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
                div [ class "absolute z-20 inset-0 bg-stone-100/40 dark:bg-stone-900/40 backdrop-blur-[2px] flex items-center justify-center cursor-pointer rounded-xl transition-all duration-300" ]
                    [ span [ class "text-lg md:text-xl font-medium text-stone-700 dark:text-stone-300 tracking-wide px-6 py-3 bg-white/80 dark:bg-stone-800/80 rounded-lg shadow-[0_2px_12px_rgb(0,0,0,0.06)] dark:shadow-none border border-stone-200/50 dark:border-stone-700/50" ] [ text "Click to start" ] ]

            else
                text ""

        firstRow =
            List.take 14 keyboard.keys
                |> List.map (viewKey keyboard.modifier)
                |> viewRow

        secondRow =
            List.drop 14 keyboard.keys
                |> List.take 13
                |> List.map (viewKey keyboard.modifier)
                |> viewRow

        thirdRow =
            List.drop 27 keyboard.keys
                |> List.take 12
                |> List.map (viewKey keyboard.modifier)
                |> viewRow

        fourthRow =
            List.drop 39 keyboard.keys
                |> List.take 5
                |> List.map (viewKey keyboard.modifier)
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


viewKey : KeyModifier -> Key -> Html msg
viewKey modifier key =
    let
        isActiveCaps =
            key.code == "CapsLock" && (modifier == CapsLock || modifier == ShiftCapsLock)

        isActiveShift =
            (key.code == "ShiftLeft" || key.code == "ShiftRight") && (modifier == Shift || modifier == ShiftCapsLock)

        bg =
            if isActiveShift && key.state /= Pressed then
                "bg-slate-300 dark:bg-slate-700 text-slate-900 dark:text-slate-100 border-t border-l border-r border-slate-400 dark:border-slate-600 " ++ fingerColorClass key.code

            else
                case key.state of
                    Pressed ->
                        "bg-slate-500 text-stone-50 dark:text-stone-950 border-b-0 translate-y-[4px]"

                    Released ->
                        "bg-stone-100 dark:bg-stone-800 text-stone-700 dark:text-stone-300 border-t border-l border-r border-stone-200 dark:border-stone-700 " ++ fingerColorClass key.code

                    Hinted ->
                        "bg-slate-200 dark:bg-slate-800/70 text-slate-900 dark:text-slate-100 border-2 border-slate-400 dark:border-slate-500 shadow-[0_0_12px_rgba(100,116,139,0.3)] animate-pulse"

        extraStyle =
            Dict.get key.code specialKeys
                |> Maybe.withDefault ""
    in
    div
        [ class <| String.join " " [ "relative z-10 x-4 py-2 text-center rounded-md shadow-[0_2px_6px_rgb(0,0,0,0.04)] dark:shadow-[0_2px_4px_rgb(0,0,0,0.2)] font-semibold w-12 transition-transform duration-75", bg, extraStyle ] ]
        [ if isActiveCaps then
            div [ class "absolute top-1.5 left-1.5 w-1.5 h-1.5 rounded-full bg-green-500 shadow-[0_0_4px_rgba(34,197,94,0.8)]" ] []

          else if key.code == "CapsLock" then
            div [ class "absolute top-1.5 left-1.5 w-1.5 h-1.5 rounded-full bg-stone-300 dark:bg-stone-600" ] []

          else
            text ""
        , text
            (if key.code == "CapsLock" then
                "Caps"

             else if key.code == "ShiftLeft" || key.code == "ShiftRight" then
                "Shift"

             else
                key.view
            )
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

