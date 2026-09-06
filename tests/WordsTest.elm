module WordsTest exposing (..)

import Dict
import Expect
import Test exposing (..)
import Words


suite : Test
suite =
    describe "Words"
        [ describe "byLesson structure"
            [ test "has entries for levels 1 through 34" <|
                \_ ->
                    let
                        keys =
                            Dict.keys Words.byLesson
                    in
                    List.range 1 34
                        |> List.all (\k -> List.member k keys)
                        |> Expect.equal True
            , test "no lesson has an empty word list" <|
                \_ ->
                    Dict.values Words.byLesson
                        |> List.all (\words -> not (List.isEmpty words))
                        |> Expect.equal True
            , test "no lesson contains empty strings" <|
                \_ ->
                    Dict.values Words.byLesson
                        |> List.all
                            (\words ->
                                List.all (\w -> String.length w > 0) words
                            )
                        |> Expect.equal True
            , test "all words contain only Ethiopic characters and spaces" <|
                \_ ->
                    Dict.values Words.byLesson
                        |> List.concatMap identity
                        |> List.all
                            (\word ->
                                String.toList word
                                    |> List.all
                                        (\c ->
                                            let
                                                code =
                                                    Char.toCode c
                                            in
                                            (code >= 0x1200 && code <= 0x137F)
                                                || (code >= 0x1380 && code <= 0x139F)
                                                || (code >= 0x2D80 && code <= 0x2DDF)
                                                || (code >= 0xAB00 && code <= 0xAB2F)
                                                || (code >= 0x1380 && code <= 0x1399)
                                                || c
                                                == ' '
                                        )
                            )
                        |> Expect.equal True
            , test "lesson count is exactly 34" <|
                \_ ->
                    Dict.size Words.byLesson
                        |> Expect.equal 34
            ]
        ]
