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
    }

viewStats : StatsData -> (List (CI.One { index : Float, record : SessionRecord } CI.Dot) -> msg) -> Html msg
viewStats data onHover =
    Html.div [ class "w-full max-w-[1000px] flex flex-col flex-1 mt-8 gap-8 pb-16" ]
        [ Html.div [ class "flex items-center justify-between" ]
            [ Html.h1 [ class "text-3xl font-bold text-stone-800 dark:text-stone-200" ] [ Html.text "Performance Stats" ]
            ]
        , Html.div [ class "w-full bg-white dark:bg-stone-800/80 rounded-2xl shadow-sm border border-stone-200 dark:border-stone-700 p-8 h-[400px] flex flex-col" ]
            [ Html.h2 [ class "text-lg font-semibold text-stone-700 dark:text-stone-300 mb-4" ] [ Html.text "Speed & Accuracy Timeline" ]
            , Html.div [ class "flex-1 w-full" ]
                [ viewTimelineChart data onHover ]
            ]
        , Html.div [ class "w-full bg-white dark:bg-stone-800/80 rounded-2xl shadow-sm border border-stone-200 dark:border-stone-700 p-8 h-[400px] flex flex-col" ]
            [ Html.h2 [ class "text-lg font-semibold text-stone-700 dark:text-stone-300 mb-4" ] [ Html.text "Letter Latency (Mastery)" ]
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
            [ C.xLabels [ CA.amount (List.length history) ]
            , C.yLabels [ CA.withGrid ]
            , C.series .index
                [ C.interpolated (\d -> toFloat d.record.wpm) [ CA.color "rgb(13, 148, 136)" ] []
                , C.interpolated (\d -> toFloat d.record.accuracy) [ CA.color "rgb(99, 102, 241)", CA.dashed [5, 5] ] []
                ]
                history
            , C.each data.hoveringStats <| \p item ->
                let
                    rec = (CI.getData item).record
                in
                [ C.tooltip item [] [] 
                    [ Html.div [ class "flex flex-col gap-1 text-sm text-stone-700" ] 
                        [ Html.div [ class "font-bold text-teal-600" ] [ Html.text ("WPM: " ++ String.fromInt rec.wpm) ]
                        , Html.div [ class "font-bold text-indigo-500" ] [ Html.text ("Accuracy: " ++ String.fromInt rec.accuracy ++ "%") ]
                        , Html.div [] [ Html.text ("Level: " ++ String.fromInt rec.lessonIdx) ]
                        , Html.div [] [ Html.text ("Errors: " ++ if List.isEmpty rec.errors then "None!" else String.join ", " rec.errors) ]
                        ]
                    ]
                ]
            ]

viewMasteryChart : Dict String LetterStat -> Html msg
viewMasteryChart letterStats =
    let
        stats =
            Dict.toList letterStats
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
            [ C.xLabels [ CA.format (\i -> 
                case List.head (List.drop (round i) stats) of
                    Just s -> s.letter
                    Nothing -> ""
                ) ]
            , C.yLabels [ CA.withGrid ]
            , C.bars
                [ CA.margin 0.1 ]
                [ C.bar (\x -> x.stat.latencyEma) [ CA.color "rgb(245, 158, 11)" ] ]
                stats
            ]
