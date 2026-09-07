module Stats exposing (AggregateStats, LetterStat, SessionRecord, StatsData, aggregateForDate, aggregateSessions, emptyAggregate, viewStats)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Html.Events
import Dict exposing (Dict)
import Time
import Set
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI

type alias AggregateStats =
    { totalDuration : Float
    , totalSessions : Int
    , topWpm : Int
    , topAccuracy : Int
    , sumWpm : Int
    , sumAccuracy : Int
    }

type alias SessionRecord =
    { timestamp : Float
    , wpm : Int
    , accuracy : Int
    , lessonIdx : Int
    , errors : List String
    , duration : Float
    , layoutKind : String
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
    , hoveringMastery : List (CI.One { index : Float, letter : String, stat : LetterStat } CI.Bar)
    , currentTime : Float
    , zone : Time.Zone
    , aggregate : AggregateStats
    }


emptyAggregate : AggregateStats
emptyAggregate =
    { totalDuration = 0
    , totalSessions = 0
    , topWpm = 0
    , topAccuracy = 0
    , sumWpm = 0
    , sumAccuracy = 0
    }


aggregateSessions : List SessionRecord -> AggregateStats
aggregateSessions =
    List.foldl
        (\record aggregate ->
            { totalDuration = aggregate.totalDuration + record.duration
            , totalSessions = aggregate.totalSessions + 1
            , topWpm = max aggregate.topWpm record.wpm
            , topAccuracy = max aggregate.topAccuracy record.accuracy
            , sumWpm = aggregate.sumWpm + record.wpm
            , sumAccuracy = aggregate.sumAccuracy + record.accuracy
            }
        )
        emptyAggregate


aggregateForDate : Time.Zone -> Float -> List SessionRecord -> AggregateStats
aggregateForDate zone currentTime history =
    let
        currentPosix =
            Time.millisToPosix (round currentTime)

        isSameDate record =
            let
                recordPosix =
                    Time.millisToPosix (round record.timestamp)
            in
            Time.toYear zone recordPosix
                == Time.toYear zone currentPosix
                && Time.toMonth zone recordPosix
                == Time.toMonth zone currentPosix
                && Time.toDay zone recordPosix
                == Time.toDay zone currentPosix
    in
    history
        |> List.filter isSameDate
        |> aggregateSessions

viewAggregateStats : String -> AggregateStats -> Html msg
viewAggregateStats title agg =
    let
        avgSpeed = if agg.totalSessions == 0 then 0 else toFloat agg.sumWpm / toFloat agg.totalSessions
        avgAccuracy = if agg.totalSessions == 0 then 0 else toFloat agg.sumAccuracy / toFloat agg.totalSessions
        
        -- format time (e.g. 00:15:25)
        h = floor (agg.totalDuration / 3600)
        m = floor (agg.totalDuration / 60) |> modBy 60
        s = floor agg.totalDuration |> modBy 60
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

        statCard label val sub =
            Html.div [ class "flex flex-col bg-slate-50 dark:bg-[#202020] rounded-lg p-4 border border-slate-100 dark:border-stone-700/50" ]
                [ Html.span [ class "text-xs font-bold tracking-wider text-slate-500 dark:text-slate-400 uppercase mb-1" ] [ Html.text label ]
                , Html.div [ class "flex items-baseline gap-2 mt-1" ]
                    [ Html.span [ class "text-3xl font-black tracking-tight text-slate-800 dark:text-slate-100" ] [ Html.text val ]
                    , if String.isEmpty sub then Html.text "" else Html.span [ class "text-sm font-semibold text-slate-500 dark:text-slate-400" ] [ Html.text sub ]
                    ]
                ]
    in
    Html.div [ class "flex-1 bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-6 flex flex-col gap-4" ]
        [ Html.h2 [ class "text-xl font-bold tracking-tight text-slate-800 dark:text-slate-100 mb-2" ] [ Html.text title ]
        , Html.div [ class "grid grid-cols-2 gap-4" ]
            [ statCard "Time" timeStr ""
            , statCard "Sessions" (String.fromInt agg.totalSessions) ""
            , statCard "Top Speed" (String.fromInt agg.topWpm) ("avg " ++ formatFloat avgSpeed ++ " wpm")
            , statCard "Accuracy" (String.fromInt agg.topAccuracy ++ "%") ("avg " ++ formatFloat avgAccuracy ++ "%")
            ]
        ]

viewStats : StatsData -> (List (CI.One { index : Float, record : SessionRecord } CI.Dot) -> msg) -> (List (CI.One { index : Float, letter : String, stat : LetterStat } CI.Bar) -> msg) -> msg -> Html msg
viewStats data onHover onHoverMastery onClickStartPractice =
    Html.div [ class "w-full max-w-[1000px] flex flex-col flex-1 mt-8 gap-8 pb-16" ]
        [ Html.div [ class "flex items-center justify-between" ]
            [ Html.h1 [ class "text-3xl font-bold text-stone-800 dark:text-stone-200" ] [ Html.text "Performance Stats" ]
            ]
        , Html.div [ class "flex flex-col md:flex-row gap-8 w-full" ]
            [ viewAggregateStats "All Time Statistics" data.aggregate
            , viewAggregateStats "Statistics for Today"
                (aggregateForDate data.zone data.currentTime data.history)
            ]
        , Html.div [ class "w-full bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-8 h-[400px] flex flex-col" ]
            [ Html.div [ class "flex flex-col md:flex-row justify-between md:items-end gap-4 mb-4" ]
                [ Html.div []
                    [ Html.h2 [ class "text-lg font-semibold text-stone-800 dark:text-stone-200" ] [ Html.text "Progress Timeline" ]
                    , Html.p [ class "text-sm text-stone-500 dark:text-stone-400 mt-1" ] [ Html.text "Speed (WPM) and Accuracy (%) across all your completed sessions." ]
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
            , Html.div [ class "flex-1 w-full", Html.Attributes.attribute "role" "img", Html.Attributes.attribute "aria-label" "Timeline chart of typing speed and accuracy" ]
                [ viewTimelineChart data onHover ]
            ]
        , Html.div [ class "w-full bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-8 h-[400px] flex flex-col" ]
            [ Html.div [ class "mb-4" ]
                [ Html.h2 [ class "text-lg font-semibold text-stone-800 dark:text-stone-200" ] [ Html.text "Slowest Characters" ]
                , Html.p [ class "text-sm text-stone-500 dark:text-stone-400 mt-1" ] [ Html.text "The average delay (in milliseconds) before you successfully type these characters. Taller bars indicate you are struggling to find them quickly." ]
                ]
            , Html.div [ class "flex-1 w-full", Html.Attributes.attribute "role" "img", Html.Attributes.attribute "aria-label" "Bar chart of slowest typed characters" ]
                [ viewMasteryChart data onHoverMastery ]
            , if (List.length data.history) >= 5 then
                Html.div [ class "mt-8 flex justify-center w-full" ]
                    [ Html.button
                        [ Html.Events.onClick onClickStartPractice
                        , class "flex items-center gap-2 bg-amber-500 hover:bg-amber-600 dark:bg-amber-600 dark:hover:bg-amber-500 text-white px-6 py-3 rounded-full font-bold shadow-md hover:shadow-lg transition-all"
                        ]
                        [ Html.text "Practice Weaknesses" ]
                    ]
              else
                Html.text ""
            ]
        ]


formatDate : Time.Zone -> Float -> String
formatDate zone ts =
    let
        posix = Time.millisToPosix (round ts)
        month = 
            case Time.toMonth zone posix of
                Time.Jan -> "Jan"
                Time.Feb -> "Feb"
                Time.Mar -> "Mar"
                Time.Apr -> "Apr"
                Time.May -> "May"
                Time.Jun -> "Jun"
                Time.Jul -> "Jul"
                Time.Aug -> "Aug"
                Time.Sep -> "Sep"
                Time.Oct -> "Oct"
                Time.Nov -> "Nov"
                Time.Dec -> "Dec"
        day = String.fromInt (Time.toDay zone posix)
    in
    month ++ " " ++ day


formatDateTime : Time.Zone -> Float -> String
formatDateTime zone ts =
    let
        posix = Time.millisToPosix (round ts)
        month = 
            case Time.toMonth zone posix of
                Time.Jan -> "Jan"
                Time.Feb -> "Feb"
                Time.Mar -> "Mar"
                Time.Apr -> "Apr"
                Time.May -> "May"
                Time.Jun -> "Jun"
                Time.Jul -> "Jul"
                Time.Aug -> "Aug"
                Time.Sep -> "Sep"
                Time.Oct -> "Oct"
                Time.Nov -> "Nov"
                Time.Dec -> "Dec"
        day = String.fromInt (Time.toDay zone posix)
        h = Time.toHour zone posix
        m = Time.toMinute zone posix
        pad n = if n < 10 then "0" ++ String.fromInt n else String.fromInt n
    in
    month ++ " " ++ day ++ " at " ++ pad h ++ ":" ++ pad m

viewTimelineChart : StatsData -> (List (CI.One { index : Float, record : SessionRecord } CI.Dot) -> msg) -> Html msg
viewTimelineChart data onHover =
    let
        history =
            List.indexedMap (\i r -> { index = toFloat i, record = r }) data.history
    in
    if List.isEmpty history then
        Html.div [ class "flex items-center justify-center h-full text-stone-500 dark:text-stone-400" ] [ Html.text "Complete a lesson to see your timeline." ]
    else
        C.chart
            [ CA.height 300
            , CA.width 900
            , CA.margin { top = 20, bottom = 45, left = 55, right = 40 }
            , CE.onMouseMove onHover (CE.getNearest CI.dots)
            , CE.onMouseLeave (onHover [])
            ]
            [ C.xLabels 
                [ CA.amount (min 6 (List.length history))
                , CA.color "var(--chart-text)"
                , CA.format (\x -> 
                    case List.head (List.drop (round x) history) of
                        Just d -> formatDate data.zone d.record.timestamp
                        Nothing -> ""
                  )
                ]
            , C.yLabels [ CA.withGrid, CA.color "var(--chart-text)" ]
            , C.grid [ CA.color "var(--chart-grid)", CA.width 1 ]
            , C.series (\d -> d.index)
                [ C.interpolated (\d -> toFloat d.record.wpm) [ CA.color "var(--chart-primary)", CA.width 3 ] []
                , C.interpolated (\d -> toFloat d.record.accuracy) [ CA.color "var(--chart-secondary)", CA.width 2, CA.dashed [6, 6] ] []
                ]
                history
            , C.each data.hoveringStats <| \p item ->
                let
                    rec = (CI.getData item).record
                in
                [ C.tooltip item [] [] 
                    [ Html.div [ class "flex flex-col gap-1 text-sm text-stone-700 dark:text-stone-300 bg-white dark:bg-stone-900 p-3 rounded-lg shadow-xl border border-stone-200 dark:border-stone-800 z-50 animate-tooltip-enter" ] 
                        [ Html.div [ class "text-xs font-semibold text-slate-400 dark:text-slate-500 mb-1" ] [ Html.text (formatDateTime data.zone rec.timestamp) ]
                        , Html.div [ class "font-bold text-slate-800 dark:text-slate-100" ] [ Html.text ("WPM: " ++ String.fromInt rec.wpm) ]
                        , Html.div [ class "font-bold text-slate-500 dark:text-slate-400" ] [ Html.text ("Accuracy: " ++ String.fromInt rec.accuracy ++ "%") ]
                        , Html.div [] [ Html.text ("Lesson: " ++ String.fromInt rec.lessonIdx) ]
                        , Html.div [] [ Html.text ("Errors: " ++ if List.isEmpty rec.errors then "None!" else String.join ", " (Set.toList (Set.fromList rec.errors))) ]
                        ]
                    ]
                ]
            ]

viewMasteryChart : StatsData -> (List (CI.One { index : Float, letter : String, stat : LetterStat } CI.Bar) -> msg) -> Html msg
viewMasteryChart data onHoverMastery =
    let
        sortedStats =
            Dict.toList data.letterStats
                |> List.filter (\(_, stat) -> stat.count > 0)
                |> List.sortBy (\(_, stat) -> -stat.latencyEma)
                |> List.take 15

        stats =
            sortedStats
                |> List.indexedMap (\i (letter, stat) -> { index = toFloat i, letter = letter, stat = stat })
    in
    if List.isEmpty stats then
        Html.div [ class "flex items-center justify-center h-full text-stone-500 dark:text-stone-400" ] [ Html.text "No data available." ]
    else
        C.chart
            [ CA.height 300
            , CA.width 900
            , CA.margin { top = 20, bottom = 45, left = 65, right = 20 }
            , CE.onMouseMove onHoverMastery (CE.getNearest CI.bars)
            , CE.onMouseLeave (onHoverMastery [])
            ]
            [ C.xLabels [ CA.amount (List.length stats), CA.color "var(--chart-text)", CA.format (\i -> 
                case List.head (List.drop (round i - 1) stats) of
                    Just s -> s.letter
                    Nothing -> ""
                ) ]
            , C.yLabels [ CA.withGrid, CA.color "var(--chart-text)", CA.format (\y -> String.fromInt (round y) ++ "ms") ]
            , C.grid [ CA.color "var(--chart-grid)", CA.width 1 ]
            , C.bars
                [ CA.margin 0.2 ]
                [ C.bar (\x -> x.stat.latencyEma) [ CA.color "var(--chart-primary)" ] ]
                stats
            , C.series (\x -> x.index)
                [ C.interpolated (\_ -> 500) [ CA.color "var(--chart-secondary)", CA.dashed [ 4, 4 ], CA.width 2 ] [] ]
                stats
            , C.each data.hoveringMastery <| \p item ->
                let
                    rec = (CI.getData item)
                in
                [ C.tooltip item [] [] 
                    [ Html.div [ class "flex flex-col gap-1 text-sm text-stone-700 dark:text-stone-300 bg-white dark:bg-stone-900 p-3 rounded-lg shadow-xl border border-stone-200 dark:border-stone-800 z-50 animate-tooltip-enter" ] 
                        [ Html.div [ class "font-bold text-slate-800 dark:text-slate-100 text-base mb-1" ] [ Html.text ("Letter: " ++ rec.letter) ]
                        , Html.div [ class "font-bold text-amber-600 dark:text-amber-500" ] [ Html.text ("Avg Delay: " ++ String.fromInt (round rec.stat.latencyEma) ++ "ms") ]
                        , Html.div [ class "font-bold text-rose-600 dark:text-rose-500" ] [ Html.text ("Error Rate: " ++ String.fromInt (round (rec.stat.errorEma * 100)) ++ "%") ]
                        , Html.div [ class "text-stone-500 dark:text-stone-400 mt-1" ] [ Html.text ("Total Typed: " ++ String.fromInt rec.stat.count ++ " times") ]
                        ]
                    ]
                ]
            ]
