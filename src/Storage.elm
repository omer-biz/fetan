module Storage exposing (..)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Types.Core exposing (..)
import Stats exposing (LetterStat, SessionRecord, AggregateStats)

keyDecoder : Decode.Decoder KeyEvent
keyDecoder =
    Decode.map2 KeyEvent
        (Decode.field "code" Decode.string)
        (Decode.field "timeStamp" Decode.float)

metricDecoder : Decode.Decoder { old : Int, new : Int }
metricDecoder =
    Decode.map2 (\o n -> { old = o, new = n })
        (Decode.field "old" Decode.int)
        (Decode.field "new" Decode.int)


metricsDecoder : Decode.Decoder Metrics
metricsDecoder =
    Decode.map2 Metrics
        (Decode.field "speed" metricDecoder)
        (Decode.field "accuracy" metricDecoder)


letterStatDecoder : Decode.Decoder LetterStat
letterStatDecoder =
    Decode.map3 LetterStat
        (Decode.field "errorEma" Decode.float)
        (Decode.field "latencyEma" Decode.float)
        (Decode.field "count" Decode.int)


statsDictDecoder : Decode.Decoder (Dict.Dict String LetterStat)
statsDictDecoder =
    Decode.dict letterStatDecoder


sessionRecordDecoder : Decode.Decoder SessionRecord
sessionRecordDecoder =
    Decode.map7 SessionRecord
        (Decode.field "timestamp" Decode.float)
        (Decode.field "wpm" Decode.int)
        (Decode.field "accuracy" Decode.int)
        (Decode.field "lessonIdx" Decode.int)
        (Decode.field "errors" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "duration" Decode.float) |> Decode.map (Maybe.withDefault 0.0))
        (Decode.maybe (Decode.field "layoutKind" Decode.string) |> Decode.map (Maybe.withDefault "Unknown"))


layoutDataDecoder : Decode.Decoder LayoutData
layoutDataDecoder =
    Decode.map3 LayoutData
        (Decode.field "lessonIdx" Decode.int |> Decode.map (\idx -> max 4 idx))
        (Decode.maybe (Decode.field "dictationsCompleted" Decode.int) |> Decode.map (Maybe.withDefault 0))
        (Decode.maybe (Decode.field "letterStats" statsDictDecoder) |> Decode.map (Maybe.withDefault Dict.empty))

layoutsDictDecoder : Decode.Decoder (Dict.Dict String LayoutData)
layoutsDictDecoder =
    Decode.dict layoutDataDecoder

aggregateDecoder : Decode.Decoder AggregateStats
aggregateDecoder =
    Decode.map6 AggregateStats
        (Decode.field "totalDuration" Decode.float)
        (Decode.field "totalSessions" Decode.int)
        (Decode.field "topWpm" Decode.int)
        (Decode.field "topAccuracy" Decode.int)
        (Decode.field "sumWpm" Decode.int)
        (Decode.field "sumAccuracy" Decode.int)

infoDecoder : Decode.Decoder Info
infoDecoder =
    Decode.value
        |> Decode.andThen
            (\val ->
                let
                    metrics =
                        Decode.decodeValue (Decode.field "metrics" metricsDecoder) val
                            |> Result.withDefault (Metrics { old = 0, new = 0 } { old = 0, new = 0 })

                    layoutKind =
                        Decode.decodeValue (Decode.field "layoutKind" Decode.string) val
                            |> Result.withDefault "GeezIME"

                    history =
                        Decode.decodeValue (Decode.field "history" (Decode.list sessionRecordDecoder)) val
                            |> Result.withDefault []

                    layouts =
                        Decode.decodeValue (Decode.field "layouts" layoutsDictDecoder) val
                            |> Result.withDefault Dict.empty

                    legacyLessonIdx =
                        Decode.decodeValue (Decode.field "lessonIdx" Decode.int) val
                            |> Result.withDefault 4
                            |> max 4

                    legacyDictationsCompleted =
                        Decode.decodeValue (Decode.field "dictationsCompleted" Decode.int) val
                            |> Result.withDefault 0

                    legacyLetterStats =
                        Decode.decodeValue (Decode.field "letterStats" statsDictDecoder) val
                            |> Result.withDefault Dict.empty
                            
                    migratedLayouts =
                        if Dict.isEmpty layouts && (legacyLessonIdx > 4 || legacyDictationsCompleted > 0 || not (Dict.isEmpty legacyLetterStats)) then
                            Dict.singleton layoutKind (LayoutData legacyLessonIdx legacyDictationsCompleted legacyLetterStats)
                        else
                            layouts
                            
                    aggregate =
                        Decode.decodeValue (Decode.field "aggregate" aggregateDecoder) val
                            |> Result.withDefault
                                (List.foldl
                                    (\r acc ->
                                        { totalDuration = acc.totalDuration + r.duration
                                        , totalSessions = acc.totalSessions + 1
                                        , topWpm = max acc.topWpm r.wpm
                                        , topAccuracy = max acc.topAccuracy r.accuracy
                                        , sumWpm = acc.sumWpm + r.wpm
                                        , sumAccuracy = acc.sumAccuracy + r.accuracy
                                        }
                                    )
                                    { totalDuration = 0, totalSessions = 0, topWpm = 0, topAccuracy = 0, sumWpm = 0, sumAccuracy = 0 }
                                    history
                                )

                    onboardingStep =
                        Decode.decodeValue (Decode.field "onboardingStep" Decode.int) val
                            |> Result.withDefault 0
                in
                Decode.succeed (Info metrics layoutKind history migratedLayouts aggregate onboardingStep)
            )


encodeMetric : { old : Int, new : Int } -> Encode.Value
encodeMetric metric =
    Encode.object [ ( "old", Encode.int metric.old ), ( "new", Encode.int metric.new ) ]


encodeMetrics : Metrics -> Encode.Value
encodeMetrics metrics =
    Encode.object
        [ ( "speed", encodeMetric metrics.speed )
        , ( "accuracy", encodeMetric metrics.accuracy )
        ]


encodeLetterStat : LetterStat -> Encode.Value
encodeLetterStat stat =
    Encode.object
        [ ( "errorEma", Encode.float stat.errorEma )
        , ( "latencyEma", Encode.float stat.latencyEma )
        , ( "count", Encode.int stat.count )
        ]


encodeSessionRecord : SessionRecord -> Encode.Value
encodeSessionRecord record =
    Encode.object
        [ ( "timestamp", Encode.float record.timestamp )
        , ( "wpm", Encode.int record.wpm )
        , ( "accuracy", Encode.int record.accuracy )
        , ( "lessonIdx", Encode.int record.lessonIdx )
        , ( "errors", Encode.list Encode.string record.errors )
        , ( "duration", Encode.float record.duration )
        , ( "layoutKind", Encode.string record.layoutKind )
        ]


encodeLayoutData : LayoutData -> Encode.Value
encodeLayoutData data =
    Encode.object
        [ ( "lessonIdx", Encode.int data.lessonIdx )
        , ( "dictationsCompleted", Encode.int data.dictationsCompleted )
        , ( "letterStats", Encode.dict identity encodeLetterStat data.letterStats )
        ]

encodeAggregate : AggregateStats -> Encode.Value
encodeAggregate agg =
    Encode.object
        [ ( "totalDuration", Encode.float agg.totalDuration )
        , ( "totalSessions", Encode.int agg.totalSessions )
        , ( "topWpm", Encode.int agg.topWpm )
        , ( "topAccuracy", Encode.int agg.topAccuracy )
        , ( "sumWpm", Encode.int agg.sumWpm )
        , ( "sumAccuracy", Encode.int agg.sumAccuracy )
        ]

encodeInfo : Info -> Encode.Value
encodeInfo info =
    Encode.object
        [ ( "metrics", encodeMetrics info.metrics )
        , ( "layoutKind", Encode.string info.layoutKind )
        , ( "layouts", Encode.dict identity encodeLayoutData info.layouts )
        , ( "history", Encode.list encodeSessionRecord info.history )
        , ( "aggregate", encodeAggregate info.aggregate )
        , ( "onboardingStep", Encode.int info.onboardingStep )
        ]


