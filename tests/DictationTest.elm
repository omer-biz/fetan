module DictationTest exposing (..)

import Dictation
import Dict
import Expect
import Random
import Test exposing (..)
import Words


suite : Test
suite =
    describe "Dictation"
        [ describe "learningSequence"
            [ test "has 34 characters" <|
                \_ ->
                    Dictation.learningSequence
                        |> List.length
                        |> Expect.equal 34
            , test "has no duplicates" <|
                \_ ->
                    let
                        len =
                            List.length Dictation.learningSequence

                        unique =
                            Dictation.learningSequence
                                |> List.map String.fromChar
                                |> List.sort
                                |> dedup
                                |> List.length
                    in
                    Expect.equal len unique
            , test "every character is an Ethiopic base letter (first form)" <|
                \_ ->
                    Dictation.learningSequence
                        |> List.all
                            (\c ->
                                let
                                    code =
                                        Char.toCode c
                                in
                                code >= 0x1200 && code <= 0x1380
                            )
                        |> Expect.equal True
            ]
        , describe "genForLevel"
            [ test "level is clamped to at least 1" <|
                \_ ->
                    let
                        result =
                            Random.step (Dictation.genForLevel 0) (Random.initialSeed 42)
                                |> Tuple.first
                    in
                    String.isEmpty result
                        |> Expect.equal False
            , test "level is clamped to at most 33" <|
                \_ ->
                    let
                        result =
                            Random.step (Dictation.genForLevel 999) (Random.initialSeed 42)
                                |> Tuple.first
                    in
                    String.isEmpty result
                        |> Expect.equal False
            , test "generates a non-empty string for level 1" <|
                \_ ->
                    let
                        result =
                            Random.step (Dictation.genForLevel 1) (Random.initialSeed 0)
                                |> Tuple.first
                    in
                    (String.length result > 0)
                        |> Expect.equal True
            , test "generated string contains spaces (multiple words)" <|
                \_ ->
                    let
                        result =
                            Random.step (Dictation.genForLevel 1) (Random.initialSeed 0)
                                |> Tuple.first
                    in
                    String.contains " " result
                        |> Expect.equal True
            , test "every lesson level 1..33 produces non-empty output" <|
                \_ ->
                    List.range 1 33
                        |> List.all
                            (\lvl ->
                                let
                                    result =
                                        Random.step (Dictation.genForLevel lvl) (Random.initialSeed lvl)
                                            |> Tuple.first
                                in
                                String.length result > 0
                            )
                        |> Expect.equal True
            ]
        , describe "Words.byLesson coverage"
            [ test "every level 1..33 has words in Words.byLesson" <|
                \_ ->
                    List.range 1 33
                        |> List.all
                            (\lvl ->
                                case Dict.get lvl Words.byLesson of
                                    Just words ->
                                        not (List.isEmpty words)

                                    Nothing ->
                                        False
                            )
                        |> Expect.equal True
            ]
        ]


dedup : List comparable -> List comparable
dedup list =
    case list of
        [] ->
            []

        [ x ] ->
            [ x ]

        x :: y :: rest ->
            if x == y then
                dedup (y :: rest)

            else
                x :: dedup (y :: rest)
