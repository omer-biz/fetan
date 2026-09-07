module UpdateTest exposing (suite)

import Dict
import Expect
import Stats exposing (LetterStat)
import Test exposing (Test, describe, test)
import Types.Core exposing (AttemptResult(..), Info)
import Update exposing (finalizeSessionInfo, nextOnboardingStep, updateLetterStat)


emptyInfo : Info
emptyInfo =
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
    , onboardingStep = 4
    }


suite : Test
suite =
    describe "Update helpers"
        [ test "a completed session updates every persisted statistic together" <|
            \_ ->
                let
                    result =
                        finalizeSessionInfo 30 123456 50 45 [ "መ" ] emptyInfo
                in
                Expect.all
                    [ \info -> Expect.equal 1 (List.length info.history)
                    , \info -> Expect.equal 1 info.aggregate.totalSessions
                    , \info -> Expect.equal 30 info.aggregate.totalDuration
                    , \info -> Expect.equal 90 info.metrics.accuracy.new
                    , \info -> Expect.equal 20 info.metrics.speed.new
                    , \info -> Expect.equal 5 info.onboardingStep
                    ]
                    result
        , test "dismissing the first tooltip advances to the next visible step" <|
            \_ ->
                nextOnboardingStep 1
                    |> Expect.equal 3
        , test "a later completed session advances the second completion tooltip" <|
            \_ ->
                { emptyInfo | onboardingStep = 6 }
                    |> finalizeSessionInfo 10 123456 10 10 []
                    |> .onboardingStep
                    |> Expect.equal 7
        , test "session history retains only the newest 200 records" <|
            \_ ->
                let
                    oldRecord index =
                        { timestamp = toFloat index
                        , wpm = index
                        , accuracy = 100
                        , lessonIdx = 4
                        , errors = []
                        , duration = 1
                        , layoutKind = "GeezIME"
                        }

                    result =
                        { emptyInfo | history = List.map oldRecord (List.range 1 205) }
                            |> finalizeSessionInfo 10 999 10 10 []
                in
                Expect.all
                    [ \info -> Expect.equal 200 (List.length info.history)
                    , \info ->
                        info.history
                            |> List.head
                            |> Maybe.map .timestamp
                            |> Expect.equal (Just 7)
                    , \info ->
                        info.history
                            |> List.reverse
                            |> List.head
                            |> Maybe.map .timestamp
                            |> Expect.equal (Just 999)
                    ]
                    result
        , test "aggregate maxima and sums include each completed session" <|
            \_ ->
                let
                    initial =
                        { emptyInfo
                            | aggregate =
                                { totalDuration = 20
                                , totalSessions = 2
                                , topWpm = 80
                                , topAccuracy = 95
                                , sumWpm = 100
                                , sumAccuracy = 180
                                }
                        }

                    result =
                        finalizeSessionInfo 30 123456 150 147 [] initial
                in
                Expect.equal
                    { totalDuration = 50
                    , totalSessions = 3
                    , topWpm = 80
                    , topAccuracy = 98
                    , sumWpm = 160
                    , sumAccuracy = 278
                    }
                    result.aggregate
        , test "the first correct attempt preserves and decays earlier errors" <|
            \_ ->
                let
                    initial : LetterStat
                    initial =
                        { errorEma = 0.1, latencyEma = 0, count = 0 }
                in
                updateLetterStat WasCorrect 250 initial
                    |> .errorEma
                    |> Expect.within (Expect.Absolute 0.000001) 0.09
        , test "repeated correct attempts use an exponential moving latency average" <|
            \_ ->
                let
                    initial : LetterStat
                    initial =
                        { errorEma = 0, latencyEma = 200, count = 4 }
                in
                updateLetterStat WasCorrect 300 initial
                    |> Expect.equal
                        { errorEma = 0
                        , latencyEma = 210
                        , count = 5
                        }
        , test "wrong attempts raise error EMA without recording a successful sample" <|
            \_ ->
                let
                    initial : LetterStat
                    initial =
                        { errorEma = 0.2, latencyEma = 250, count = 3 }
                in
                updateLetterStat WasWrong 999 initial
                    |> Expect.equal
                        { errorEma = 0.28
                        , latencyEma = 250
                        , count = 3
                        }
        , test "partial and no-op attempts leave letter statistics unchanged" <|
            \_ ->
                let
                    initial : LetterStat
                    initial =
                        { errorEma = 0.2, latencyEma = 250, count = 3 }
                in
                [ updateLetterStat WasPartial 100 initial
                , updateLetterStat NoOpResult 100 initial
                ]
                    |> Expect.equal [ initial, initial ]
        ]
