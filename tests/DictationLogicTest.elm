module DictationLogicTest exposing (..)

import Dict
import DictationLogic exposing (getFamilyStats, stringToDictation)
import Expect
import Stats exposing (LetterStat)
import Test exposing (..)
import Types.Core exposing (Dictation, Letter, LetterState(..))

suite : Test
suite =
    describe "DictationLogic"
        [ describe "stringToDictation"
            [ test "empty string" <|
                \_ ->
                    stringToDictation ""
                        |> Expect.equal (Dictation [] Nothing [])
            , test "single char string" <|
                \_ ->
                    stringToDictation "a"
                        |> Expect.equal (Dictation [] (Just (Letter 'a' Fresh False)) [])
            , test "multi char string" <|
                \_ ->
                    stringToDictation "abc"
                        |> Expect.equal
                            (Dictation []
                                (Just (Letter 'a' Fresh False))
                                [ Letter 'b' Fresh False, Letter 'c' Fresh False ]
                            )
            ]
        , describe "getFamilyStats"
            [ test "aggregates family stats correctly" <|
                \_ ->
                    let
                        stats =
                            Dict.fromList
                                [ ( "መ", LetterStat 10.0 5.0 10 )
                                , ( "ሙ", LetterStat 20.0 15.0 10 )
                                , ( "ሚ", LetterStat 0.0 0.0 0 )
                                , ( "ማ", LetterStat 0.0 0.0 0 )
                                , ( "ሜ", LetterStat 0.0 0.0 0 )
                                , ( "ም", LetterStat 0.0 0.0 0 )
                                , ( "ሞ", LetterStat 0.0 0.0 0 )
                                , ( "ሟ", LetterStat 0.0 0.0 0 )
                                ]

                        result =
                            getFamilyStats "መ" stats
                    in
                    Expect.all
                        [ \r -> Expect.equal 20 r.count
                        , \r -> Expect.equal 10.0 r.latencyEma
                        , \r -> Expect.equal 15.0 r.errorEma
                        ]
                        result
            ]
        ]
