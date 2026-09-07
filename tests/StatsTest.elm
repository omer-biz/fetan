module StatsTest exposing (suite)

import Expect
import Stats exposing (SessionRecord, aggregateForDate, aggregateSessions, emptyAggregate)
import Test exposing (Test, describe, test)
import Time


session : Float -> Int -> Int -> Float -> SessionRecord
session timestamp wpm accuracy duration =
    { timestamp = timestamp
    , wpm = wpm
    , accuracy = accuracy
    , lessonIdx = 4
    , errors = []
    , duration = duration
    , layoutKind = "GeezIME"
    }


suite : Test
suite =
    describe "statistics aggregation"
        [ test "an empty history has an empty aggregate" <|
            \_ ->
                aggregateSessions []
                    |> Expect.equal emptyAggregate
        , test "totals, sums, and maxima are accumulated independently" <|
            \_ ->
                aggregateSessions
                    [ session 1 40 97 12.5
                    , session 2 55 91 17.5
                    , session 3 48 99 10
                    ]
                    |> Expect.equal
                        { totalDuration = 40
                        , totalSessions = 3
                        , topWpm = 55
                        , topAccuracy = 99
                        , sumWpm = 143
                        , sumAccuracy = 287
                        }
        , test "today uses the user's local date across a UTC boundary" <|
            \_ ->
                let
                    addisAbaba =
                        Time.customZone 180 []

                    currentTime =
                        1788741000000

                    lateUtcPreviousDay =
                        session 1788732000000 60 98 20

                    earlierUtcPreviousDay =
                        session 1788724800000 30 90 10
                in
                aggregateForDate addisAbaba currentTime [ lateUtcPreviousDay, earlierUtcPreviousDay ]
                    |> Expect.equal
                        { totalDuration = 20
                        , totalSessions = 1
                        , topWpm = 60
                        , topAccuracy = 98
                        , sumWpm = 60
                        , sumAccuracy = 98
                        }
        ]
