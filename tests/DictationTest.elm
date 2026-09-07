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
            , test "level is clamped to the final lesson" <|
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
            , test "the final lesson uses the final lesson's word list" <|
                \_ ->
                    let
                        result =
                            Random.step (Dictation.genForLevel Dictation.lessonCount) (Random.initialSeed 42)
                                |> Tuple.first

                        finalWords =
                            Dict.get Dictation.lessonCount Words.byLesson
                                |> Maybe.withDefault []
                    in
                    result
                        |> String.words
                        |> List.all (\word -> List.member word finalWords)
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
            , test "every lesson produces non-empty output" <|
                \_ ->
                    List.range 1 Dictation.lessonCount
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
            [ test "every lesson has words in Words.byLesson" <|
                \_ ->
                    List.range 1 Dictation.lessonCount
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
        , describe "genForWeaknesses"
            [ test "every generated word targets an available weak character" <|
                \_ ->
                    let
                        result =
                            Random.step
                                (Dictation.genForWeaknesses [ "ረ" ] 5)
                                (Random.initialSeed 17)
                                |> Tuple.first

                        availableWords =
                            Dict.toList Words.byLesson
                                |> List.filter (\( level, _ ) -> level <= 5)
                                |> List.concatMap Tuple.second
                                |> List.filter (String.contains "ረ")
                    in
                    result
                        |> String.words
                        |> List.all (\word -> List.member word availableWords)
                        |> Expect.equal True
            , test "an empty weakness list falls back to unlocked lesson words" <|
                \_ ->
                    let
                        result =
                            Random.step
                                (Dictation.genForWeaknesses [] 4)
                                (Random.initialSeed 21)
                                |> Tuple.first

                        availableWords =
                            Dict.toList Words.byLesson
                                |> List.filter (\( level, _ ) -> level <= 4)
                                |> List.concatMap Tuple.second
                    in
                    result
                        |> String.words
                        |> List.all (\word -> List.member word availableWords)
                        |> Expect.equal True
            , test "weakness generation clamps access at the final lesson" <|
                \_ ->
                    Random.step
                        (Dictation.genForWeaknesses [ "ቨ" ] 999)
                        (Random.initialSeed 42)
                        |> Tuple.first
                        |> String.isEmpty
                        |> Expect.equal False
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
