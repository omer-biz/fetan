module Stats exposing (SessionRecord, LetterStat, StatsData, viewStats)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Dict exposing (Dict)
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI

type alias SessionRecord =
    { timestamp : Float
    , wpm : Int
    , accuracy : Int
    , lessonIdx : Int
    , errors : List String
    , duration : Float
    }

type alias LetterStat =
    { errorEma : Float
    , latencyEma : Float
    , count : Int
    }

type alias StatsData =
    { history : List SessionRecord
    , letterStats : Dict String LetterStat
    , hoveringStats : List (CI.One { index : Float, record : SessionRecord } CI.Dot)
    , currentTime : Float
    }

viewAggregateStats : String -> List SessionRecord -> Html msg
viewAggregateStats title history =
    let
        totalDuration = List.map .duration history |> List.sum
        totalLessons = List.length history
        topSpeed = List.map .wpm history |> List.maximum |> Maybe.withDefault 0
        avgSpeed = if totalLessons == 0 then 0 else (List.map .wpm history |> List.sum |> toFloat) / toFloat totalLessons
        topAccuracy = List.map .accuracy history |> List.maximum |> Maybe.withDefault 0
        avgAccuracy = if totalLessons == 0 then 0 else (List.map .accuracy history |> List.sum |> toFloat) / toFloat totalLessons

        -- format time (e.g. 00:15:25)
        h = floor (totalDuration / 3600)
        m = floor (totalDuration / 60) |> modBy 60
        s = floor totalDuration |> modBy 60
        pad n = if n < 10 then "0" ++ String.fromInt n else String.fromInt n
        timeStr = pad h ++ ":" ++ pad m ++ ":" ++ pad s

        formatFloat f =
            let
                str = String.fromFloat (toFloat (round (f * 10)) / 10)
            in
            if String.contains "." str then
                str
            else
                str ++ ".0"

        statRow label val =
            Html.div [ class "flex justify-between items-center text-sm" ]
                [ Html.span [ class "text-stone-500 dark:text-stone-400 font-medium" ] [ Html.text label ]
                , Html.span [ class "text-stone-800 dark:text-stone-200 font-bold" ] [ Html.text val ]
                ]
    in
    Html.div [ class "flex-1 bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-6 flex flex-col gap-3" ]
        [ Html.h2 [ class "text-lg font-bold text-stone-700 dark:text-stone-300 mb-2 border-b border-stone-200 dark:border-stone-700 pb-2" ] [ Html.text title ]
        , statRow "Time:" timeStr
        , statRow "Lessons:" (String.fromInt totalLessons)
        , statRow "Top speed:" (String.fromInt topSpeed ++ "wpm")
        , statRow "Average speed:" (formatFloat avgSpeed ++ "wpm")
        , statRow "Top accuracy:" (String.fromInt topAccuracy ++ "%")
        , statRow "Average accuracy:" (formatFloat avgAccuracy ++ "%")
        ]

viewStats : StatsData -> (List (CI.One { index : Float, record : SessionRecord } CI.Dot) -> msg) -> Html msg
viewStats data onHover =
    Html.div [ class "w-full max-w-[1000px] flex flex-col flex-1 mt-8 gap-8 pb-16" ]
        [ Html.div [ class "flex items-center justify-between" ]
            [ Html.h1 [ class "text-3xl font-bold text-stone-800 dark:text-stone-200" ] [ Html.text "Performance Stats" ]
            ]
        , Html.div [ class "flex flex-col md:flex-row gap-8 w-full" ]
            [ viewAggregateStats "All Time Statistics" data.history
            , viewAggregateStats "Statistics for Today" (List.filter (\r -> data.currentTime - r.timestamp < 86400000) data.history)
            ]
        , Html.div [ class "w-full bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-8 h-[400px] flex flex-col" ]
            [ Html.div [ class "flex justify-between items-end mb-4" ]
                [ Html.div []
                    [ Html.h2 [ class "text-lg font-semibold text-stone-800 dark:text-stone-200" ] [ Html.text "Progress Timeline" ]
                    , Html.p [ class "text-sm text-stone-500 dark:text-stone-400 mt-1" ] [ Html.text "Speed (WPM) and Accuracy (%) across all your completed lessons." ]
                    ]
                , Html.div [ class "flex gap-4 text-sm font-medium" ]
                    [ Html.div [ class "flex items-center gap-2" ]
                        [ Html.div [ class "w-3 h-3 rounded-full bg-slate-700 dark:bg-slate-300" ] []
                        , Html.span [ class "text-stone-600 dark:text-stone-300" ] [ Html.text "WPM" ]
                        ]
                    , Html.div [ class "flex items-center gap-2" ]
                        [ Html.div [ class "w-3 h-0.5 border-t-2 border-dashed border-slate-400 dark:border-slate-600" ] []
                        , Html.span [ class "text-stone-600 dark:text-stone-300" ] [ Html.text "Accuracy %" ]
                        ]
                    ]
                ]
            , Html.div [ class "flex-1 w-full" ]
                [ viewTimelineChart data onHover ]
            ]
        , Html.div [ class "w-full bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-8 h-[400px] flex flex-col" ]
            [ Html.div [ class "mb-4" ]
                [ Html.h2 [ class "text-lg font-semibold text-stone-800 dark:text-stone-200" ] [ Html.text "Slowest Characters" ]
                , Html.p [ class "text-sm text-stone-500 dark:text-stone-400 mt-1" ] [ Html.text "The average delay (in milliseconds) before you successfully type these characters. Taller bars indicate you are struggling to find them quickly." ]
                ]
            , Html.div [ class "flex-1 w-full" ]
                [ viewMasteryChart data.letterStats ]
            ]
        ]

viewTimelineChart : StatsData -> (List (CI.One { index : Float, record : SessionRecord } CI.Dot) -> msg) -> Html msg
viewTimelineChart data onHover =
    let
        history =
            List.indexedMap (\i r -> { index = toFloat i, record = r }) data.history
    in
    if List.isEmpty history then
        Html.div [ class "flex items-center justify-center h-full text-stone-500" ] [ Html.text "Complete a lesson to see your timeline." ]
    else
        C.chart
            [ CA.height 300
            , CA.width 900
            , CA.margin { top = 20, bottom = 30, left = 40, right = 40 }
            , CE.onMouseMove onHover (CE.getNearest CI.dots)
            , CE.onMouseLeave (onHover [])
            ]
            [ C.xLabels [ CA.amount (List.length history), CA.color "var(--chart-text)", CA.format (\i -> "L" ++ String.fromInt (round i)) ]
            , C.yLabels [ CA.withGrid, CA.color "var(--chart-text)" ]
            , C.series .index
                [ C.interpolated (\d -> toFloat d.record.wpm) [ CA.color "var(--chart-primary)", CA.width 3 ] []
                , C.interpolated (\d -> toFloat d.record.accuracy) [ CA.color "var(--chart-secondary)", CA.width 2, CA.dashed [6, 6] ] []
                ]
                history
            , C.each data.hoveringStats <| \p item ->
                let
                    rec = (CI.getData item).record
                in
                [ C.tooltip item [] [] 
                    [ Html.div [ class "flex flex-col gap-1 text-sm text-stone-700 dark:text-stone-300 bg-white dark:bg-stone-900 p-3 rounded-lg shadow-xl border border-stone-200 dark:border-stone-800" ] 
                        [ Html.div [ class "font-bold text-slate-800 dark:text-slate-100" ] [ Html.text ("WPM: " ++ String.fromInt rec.wpm) ]
                        , Html.div [ class "font-bold text-slate-500 dark:text-slate-400" ] [ Html.text ("Accuracy: " ++ String.fromInt rec.accuracy ++ "%") ]
                        , Html.div [] [ Html.text ("Level: " ++ String.fromInt rec.lessonIdx) ]
                        , Html.div [] [ Html.text ("Errors: " ++ if List.isEmpty rec.errors then "None!" else String.join ", " rec.errors) ]
                        ]
                    ]
                ]
            ]

viewMasteryChart : Dict String LetterStat -> Html msg
viewMasteryChart letterStats =
    let
        sortedStats =
            Dict.toList letterStats
                |> List.filter (\(_, stat) -> stat.count > 0)
                |> List.sortBy (\(_, stat) -> -stat.latencyEma)
                |> List.take 15

        stats =
            sortedStats
                |> List.indexedMap (\i (letter, stat) -> { index = toFloat i, letter = letter, stat = stat })
    in
    if List.isEmpty stats then
        Html.div [ class "flex items-center justify-center h-full text-stone-500" ] [ Html.text "No data available." ]
    else
        C.chart
            [ CA.height 300
            , CA.width 900
            , CA.margin { top = 20, bottom = 30, left = 40, right = 20 }
            ]
            [ C.xLabels [ CA.color "var(--chart-text)", CA.format (\i -> 
                case List.head (List.drop (round i) stats) of
                    Just s -> s.letter
                    Nothing -> ""
                ) ]
            , C.yLabels [ CA.withGrid, CA.color "var(--chart-text)", CA.format (\y -> String.fromInt (round y) ++ "ms") ]
            , C.bars
                [ CA.margin 0.2 ]
                [ C.bar (\x -> x.stat.latencyEma) [ CA.color "var(--chart-primary)" ] ]
                stats
            ]
