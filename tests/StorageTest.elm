module StorageTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Stats exposing (LetterStat, SessionRecord)
import Storage exposing (..)
import Test exposing (..)
import Types.Core exposing (Info, Metrics)

sampleLetterStat : LetterStat
sampleLetterStat =
    { errorEma = 0.5, latencyEma = 150.0, count = 10 }

sampleSession : SessionRecord
sampleSession =
    { timestamp = 1600000.0
    , wpm = 40
    , accuracy = 98
    , lessonIdx = 5
    , errors = []
    , duration = 15.0
    }

sampleMetrics : Metrics
sampleMetrics =
    { speed = { old = 35, new = 40 }
    , accuracy = { old = 95, new = 98 }
    }

sampleInfo : Info
sampleInfo =
    { lessonIdx = 5
    , layoutKind = "GeezIME"
    , dictationsCompleted = 100
    , metrics = sampleMetrics
    , letterStats = Dict.fromList [ ( "ሀ", sampleLetterStat ) ]
    , history = [ sampleSession ]
    }

suite : Test
suite =
    describe "Storage (Encoders and Decoders)"
        [ test "Info roundtrip" <|
            \_ ->
                sampleInfo
                    |> encodeInfo
                    |> Decode.decodeValue infoDecoder
                    |> Expect.equal (Ok sampleInfo)
        ]
