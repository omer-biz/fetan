module Layouts.SilPowerG exposing (Model, layout)

import Dict exposing (Dict)
import Layout exposing (KeyAttempt(..), KeyModifier(..))


type alias Model =
    Maybe Char


layout : Layout.Layout Model
layout =
    Layout.Layout pointToLetter keyAttempt Nothing


silPowerGKeys : Dict String ( Char, Char )
silPowerGKeys =
    Dict.fromList
        [ ( "Backquote", ( '`', '~' ) )
        , ( "Digit1", ( '1', '!' ) )
        , ( "Digit2", ( '2', '@' ) )
        , ( "Digit3", ( '3', '#' ) )
        , ( "Digit4", ( '4', '$' ) )
        , ( "Digit5", ( '5', '%' ) )
        , ( "Digit6", ( '6', '^' ) )
        , ( "Digit7", ( '7', '&' ) )
        , ( "Digit8", ( '8', '*' ) )
        , ( "Digit9", ( '9', '(' ) )
        , ( "Digit0", ( '0', ')' ) )
        , ( "Minus", ( '-', '_' ) )
        , ( "Equal", ( '=', '+' ) )
        , ( "KeyQ", ( 'ቀ', 'ቐ' ) )
        , ( "KeyW", ( 'ወ', 'ኧ' ) )
        , ( "KeyE", ( 'እ', 'ዕ' ) )
        , ( "KeyR", ( 'ረ', ' ' ) )
        , ( "KeyT", ( 'ተ', 'ጠ' ) )
        , ( "KeyY", ( 'ኤ', 'የ' ) )
        , ( "KeyU", ( 'ኡ', 'ዑ' ) )
        , ( "KeyI", ( 'ኢ', 'ዒ' ) )
        , ( "KeyO", ( 'ኦ', 'ዖ' ) )
        , ( "KeyP", ( 'ፐ', 'ጰ' ) )
        , ( "BracketLeft", ( '[', '{' ) )
        , ( "BracketRight", ( ']', '}' ) )
        , ( "Backslash", ( '\\', '|' ) )
        , ( "KeyA", ( 'ኣ', 'ዓ' ) )
        , ( "KeyS", ( 'ሰ', 'ሸ' ) )
        , ( "KeyD", ( 'ደ', 'ዸ' ) )
        , ( "KeyF", ( 'ፈ', 'ጸ' ) )
        , ( "KeyG", ( 'ገ', 'ጘ' ) )
        , ( "KeyH", ( 'ሀ', 'ሐ' ) )
        , ( "KeyJ", ( 'ጀ', 'ሠ' ) )
        , ( "KeyK", ( 'ከ', 'ኸ' ) )
        , ( "KeyL", ( 'ለ', 'ኀ' ) )
        , ( "Semicolon", ( '፤', '፡' ) )
        , ( "Quote", ( '\'', '"' ) )
        , ( "KeyZ", ( 'ዘ', 'ዠ' ) )
        , ( "KeyX", ( 'አ', 'ዐ' ) )
        , ( "KeyC", ( 'ቸ', 'ጨ' ) )
        , ( "KeyV", ( 'ቨ', 'ፀ' ) )
        , ( "KeyB", ( 'በ', '\u{0000}' ) )
        , ( "KeyN", ( 'ነ', 'ኘ' ) )
        , ( "KeyM", ( 'መ', '\u{0000}' ) )
        , ( "Comma", ( '፥', '<' ) )
        , ( "Period", ( '።', '>' ) )
        , ( "Slash", ( '/', '?' ) )
        ]


pointToChar : KeyModifier -> String -> Char
pointToChar keybrState codePoint =
    let
        tupleSelector =
            case keybrState of
                NoModifier ->
                    Tuple.first

                _ ->
                    Tuple.second
    in
    Dict.get codePoint silPowerGKeys
        |> Maybe.map tupleSelector
        |> Maybe.withDefault '\u{0000}'


pointToLetter : KeyModifier -> String -> String
pointToLetter keybrState codePoint =
    pointToChar keybrState codePoint |> String.fromChar


keyAttempt : KeyModifier -> String -> Char -> Model -> KeyAttempt Model
keyAttempt keybrState codePoint currentLetter partial =
    let
        attempt =
            pointToChar keybrState codePoint

        clUnicode =
            Char.toCode currentLetter

        attemptUnicode =
            Char.toCode attempt

        checkPartial p =
            if Char.toCode p + (attemptUnicode - 0x12A0) == clUnicode then
                Correct

            else
                Wrong
    in
    if attempt == currentLetter then
        Correct

    else if attemptUnicode >= 0x12A1 && attemptUnicode <= 0x12A7 then
        partial
            |> Maybe.map checkPartial
            |> Maybe.withDefault Wrong

    else if (clUnicode - attemptUnicode) > 0 && (clUnicode - attemptUnicode) <= 7 then
        Partial <| Just attempt

    else
        Wrong
