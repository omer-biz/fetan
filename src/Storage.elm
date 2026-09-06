module Storage exposing (..)

import Dict
import Json.Decode as Decode
import Json.Encode as Encode
import Types.Core exposing (..)
import Stats exposing (LetterStat, SessionRecord)

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
    Decode.map6 SessionRecord
        (Decode.field "timestamp" Decode.float)
        (Decode.field "wpm" Decode.int)
        (Decode.field "accuracy" Decode.int)
        (Decode.field "lessonIdx" Decode.int)
        (Decode.field "errors" (Decode.list Decode.string))
        (Decode.maybe (Decode.field "duration" Decode.float) |> Decode.map (Maybe.withDefault 0.0))


infoDecoder : Decode.Decoder Info
infoDecoder =
    Decode.map6 Info
        (Decode.field "metrics" metricsDecoder)
        (Decode.field "lessonIdx" Decode.int |> Decode.map (\idx -> max 4 idx))
        (Decode.maybe (Decode.field "layoutKind" Decode.string) |> Decode.map (Maybe.withDefault "GeezIME"))
        (Decode.maybe (Decode.field "dictationsCompleted" Decode.int) |> Decode.map (Maybe.withDefault 0))
        (Decode.maybe (Decode.field "letterStats" statsDictDecoder) |> Decode.map (Maybe.withDefault Dict.empty))
        (Decode.maybe (Decode.field "history" (Decode.list sessionRecordDecoder)) |> Decode.map (Maybe.withDefault []))


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
        ]


encodeInfo : Info -> Encode.Value
encodeInfo info =
    Encode.object
        [ ( "metrics", encodeMetrics info.metrics )
        , ( "lessonIdx", Encode.int info.lessonIdx )
        , ( "layoutKind", Encode.string info.layoutKind )
        , ( "dictationsCompleted", Encode.int info.dictationsCompleted )
        , ( "letterStats", Encode.dict identity encodeLetterStat info.letterStats )
        , ( "history", Encode.list encodeSessionRecord info.history )
        ]


