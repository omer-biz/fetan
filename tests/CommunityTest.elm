module CommunityTest exposing (suite)

import Community exposing (Status(..))
import Expect
import Json.Encode as Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "community responses"
        [ test "successful responses load sessions" <|
            \_ ->
                let
                    session =
                        Encode.object
                            [ ( "wpm", Encode.int 42 )
                            , ( "accuracy", Encode.int 98 )
                            , ( "duration", Encode.float 30 )
                            , ( "lessonIdx", Encode.int 4 )
                            , ( "slowestLetter", Encode.string "መ" )
                            , ( "timestamp", Encode.string "2026-09-07T12:00:00Z" )
                            ]

                    response =
                        Encode.object
                            [ ( "ok", Encode.bool True )
                            , ( "sessions", Encode.list identity [ session ] )
                            ]
                in
                case (Community.handleReceiveStats response Community.init).status of
                    Loaded sessions ->
                        Expect.equal 1 (List.length sessions)

                    _ ->
                        Expect.fail "Expected a loaded response"
        , test "failed responses retain an actionable error" <|
            \_ ->
                let
                    response =
                        Encode.object
                            [ ( "ok", Encode.bool False )
                            , ( "error", Encode.string "Temporarily unavailable" )
                            ]
                in
                (Community.handleReceiveStats response Community.init).status
                    |> Expect.equal (Error "Temporarily unavailable")
        ]
