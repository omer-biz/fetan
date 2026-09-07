module LayoutBehaviorTest exposing (suite)

import Dict
import Expect
import Layouts.PowerGeez as PowerGeez
import Layouts.SilPowerG as SilPowerG
import Models.Layout as Layout
import Set
import Test exposing (Test, describe, test)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))
import Words


lessonCharacters : List Char
lessonCharacters =
    Words.byLesson
        |> Dict.values
        |> List.concat
        |> String.concat
        |> String.toList
        |> List.filter ((/=) ' ')
        |> Set.fromList
        |> Set.toList


canTypeWithHints : Int -> Char -> Layout.Layout -> Bool
canTypeWithHints remaining target layout =
    if remaining <= 0 then
        False

    else
        case Layout.hint target layout of
            Nothing ->
                False

            Just ( modifier, code ) ->
                case Layout.update modifier code target layout of
                    ( _, Correct ) ->
                        True

                    ( nextLayout, Partial ) ->
                        canTypeWithHints (remaining - 1) target nextLayout

                    _ ->
                        False


unreachableCharacters : Layout.LayoutKind -> List String
unreachableCharacters kind =
    lessonCharacters
        |> List.filter (\character -> not (canTypeWithHints 8 character (Layout.initLayout kind)))
        |> List.map String.fromChar


suite : Test
suite =
    describe "physical keyboard layouts"
        [ test "SIL Power-G types a base character" <|
            \_ ->
                SilPowerG.update NoModifier "KeyH" 'ሀ' SilPowerG.init
                    |> Tuple.second
                    |> Expect.equal Correct
        , test "SIL Power-G completes a base and vowel sequence" <|
            \_ ->
                let
                    ( partialModel, firstAttempt ) =
                        SilPowerG.update NoModifier "KeyH" 'ሁ' SilPowerG.init

                    secondAttempt =
                        SilPowerG.update NoModifier "KeyU" 'ሁ' partialModel
                            |> Tuple.second
                in
                Expect.equal ( Partial, Correct ) ( firstAttempt, secondAttempt )
        , test "PowerGeez exposes Caps Lock characters" <|
            \_ ->
                PowerGeez.update CapsLock "KeyS" 'ሸ' PowerGeez.init
                    |> Tuple.second
                    |> Expect.equal Correct
        , test "PowerGeez exposes Shift characters" <|
            \_ ->
                PowerGeez.update Shift "KeyH" 'ሐ' PowerGeez.init
                    |> Tuple.second
                    |> Expect.equal Correct
        , test "GeezIME hints can type every character used by the curriculum" <|
            \_ ->
                unreachableCharacters Layout.GeezIME
                    |> Expect.equal []
        , test "SIL Power-G hints can type every character used by the curriculum" <|
            \_ ->
                unreachableCharacters Layout.SilPowerG
                    |> Expect.equal []
        , test "PowerGeez hints can type every character used by the curriculum" <|
            \_ ->
                unreachableCharacters Layout.PowerGeez
                    |> Expect.equal []
        ]
