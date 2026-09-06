module StorageTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Stats exposing (LetterStat, SessionRecord)
import Storage exposing (..)
import Test exposing (..)
import Types.Core exposing (Info, Metrics, LayoutData)

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
    , layoutKind = "GeezIME"
    }

sampleMetrics : Metrics
sampleMetrics =
    { speed = { old = 35, new = 40 }
    , accuracy = { old = 95, new = 98 }
    }

sampleLayoutData : LayoutData
sampleLayoutData =
    { lessonIdx = 5
    , dictationsCompleted = 100
    , letterStats = Dict.fromList [ ( "ሀ", sampleLetterStat ) ]
    }

sampleInfo : Info
sampleInfo =
    { metrics = sampleMetrics
    , layoutKind = "GeezIME"
    , history = [ sampleSession ]
    , layouts = Dict.fromList [ ( "GeezIME", sampleLayoutData ) ]
    , aggregate = { totalDuration = 15.0, totalSessions = 1, topWpm = 40, topAccuracy = 98, sumWpm = 40, sumAccuracy = 98 }
    , onboardingStep = 4
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
