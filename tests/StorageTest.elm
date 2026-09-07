module StorageTest exposing (..)

import Dict
import Expect
import Json.Decode as Decode
import Json.Encode as Encode
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
        , test "legacy invisible onboarding step migrates to a visible step" <|
            \_ ->
                sampleInfo
                    |> (\info -> { info | onboardingStep = 2 })
                    |> encodeInfo
                    |> Decode.decodeValue infoDecoder
                    |> Result.map .onboardingStep
                    |> Expect.equal (Ok 3)
        , test "missing optional fields decode to safe defaults" <|
            \_ ->
                Encode.object []
                    |> Decode.decodeValue infoDecoder
                    |> Expect.equal
                        (Ok
                            { metrics =
                                { speed = { old = 0, new = 0 }
                                , accuracy = { old = 0, new = 0 }
                                }
                            , layoutKind = "GeezIME"
                            , history = []
                            , layouts = Dict.empty
                            , aggregate =
                                { totalDuration = 0
                                , totalSessions = 0
                                , topWpm = 0
                                , topAccuracy = 0
                                , sumWpm = 0
                                , sumAccuracy = 0
                                }
                            , onboardingStep = 0
                            }
                        )
        , test "legacy flat progress migrates into the selected layout" <|
            \_ ->
                Encode.object
                    [ ( "layoutKind", Encode.string "PowerGeez" )
                    , ( "lessonIdx", Encode.int 9 )
                    , ( "dictationsCompleted", Encode.int 12 )
                    , ( "letterStats", Encode.object [ ( "ሀ", encodeLetterStat sampleLetterStat ) ] )
                    ]
                    |> Decode.decodeValue infoDecoder
                    |> Result.map (.layouts >> Dict.get "PowerGeez")
                    |> Expect.equal
                        (Ok
                            (Just
                                { lessonIdx = 9
                                , dictationsCompleted = 12
                                , letterStats = Dict.singleton "ሀ" sampleLetterStat
                                }
                            )
                        )
        , test "history rebuilds a missing aggregate during migration" <|
            \_ ->
                Encode.object
                    [ ( "history", Encode.list encodeSessionRecord [ sampleSession ] ) ]
                    |> Decode.decodeValue infoDecoder
                    |> Result.map .aggregate
                    |> Expect.equal
                        (Ok
                            { totalDuration = 15
                            , totalSessions = 1
                            , topWpm = 40
                            , topAccuracy = 98
                            , sumWpm = 40
                            , sumAccuracy = 98
                            }
                        )
        , test "persisted onboarding steps are clamped into the visible range" <|
            \_ ->
                [ -5, 99 ]
                    |> List.map
                        (\step ->
                            sampleInfo
                                |> (\info -> { info | onboardingStep = step })
                                |> encodeInfo
                                |> Decode.decodeValue infoDecoder
                                |> Result.map .onboardingStep
                        )
                    |> Expect.equal [ Ok 0, Ok 8 ]
        , test "persisted lesson progress cannot fall below the first usable lesson" <|
            \_ ->
                sampleInfo
                    |> (\info ->
                            { info
                                | layouts =
                                    Dict.singleton "GeezIME"
                                        { sampleLayoutData | lessonIdx = -20 }
                            }
                       )
                    |> encodeInfo
                    |> Decode.decodeValue infoDecoder
                    |> Result.map (.layouts >> Dict.get "GeezIME" >> Maybe.map .lessonIdx)
                    |> Expect.equal (Ok (Just 4))
        ]
