module LayoutTest exposing (..)

import Expect
import Layouts.GeezIME as GeezIME
import Models.Layout as Layout
import Test exposing (..)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


suite : Test
suite =
    describe "Models.Layout"
        [ describe "modifier state machine"
            [ test "shiftDown from NoModifier -> Shift" <|
                \_ ->
                    Layout.shiftDown NoModifier
                        |> Expect.equal Shift
            , test "shiftDown from CapsLock -> ShiftCapsLock" <|
                \_ ->
                    Layout.shiftDown CapsLock
                        |> Expect.equal ShiftCapsLock
            , test "shiftDown from Shift -> Shift (idempotent)" <|
                \_ ->
                    Layout.shiftDown Shift
                        |> Expect.equal Shift
            , test "shiftDown from ShiftCapsLock -> ShiftCapsLock (idempotent)" <|
                \_ ->
                    Layout.shiftDown ShiftCapsLock
                        |> Expect.equal ShiftCapsLock
            , test "shiftUp from Shift -> NoModifier" <|
                \_ ->
                    Layout.shiftUp Shift
                        |> Expect.equal NoModifier
            , test "shiftUp from ShiftCapsLock -> CapsLock" <|
                \_ ->
                    Layout.shiftUp ShiftCapsLock
                        |> Expect.equal CapsLock
            , test "shiftUp from NoModifier -> NoModifier (idempotent)" <|
                \_ ->
                    Layout.shiftUp NoModifier
                        |> Expect.equal NoModifier
            , test "shiftUp from CapsLock -> CapsLock (idempotent)" <|
                \_ ->
                    Layout.shiftUp CapsLock
                        |> Expect.equal CapsLock
            , test "capsFlip from NoModifier -> CapsLock" <|
                \_ ->
                    Layout.capsFlip NoModifier
                        |> Expect.equal CapsLock
            , test "capsFlip from CapsLock -> NoModifier" <|
                \_ ->
                    Layout.capsFlip CapsLock
                        |> Expect.equal NoModifier
            , test "capsFlip from Shift -> ShiftCapsLock" <|
                \_ ->
                    Layout.capsFlip Shift
                        |> Expect.equal ShiftCapsLock
            , test "capsFlip from ShiftCapsLock -> Shift" <|
                \_ ->
                    Layout.capsFlip ShiftCapsLock
                        |> Expect.equal Shift
            , test "capsFlip is its own inverse" <|
                \_ ->
                    let
                        allMods =
                            [ NoModifier, Shift, CapsLock, ShiftCapsLock ]
                    in
                    allMods
                        |> List.all (\m -> Layout.capsFlip (Layout.capsFlip m) == m)
                        |> Expect.equal True
            ]
        , describe "keyModDown / keyModUp"
            [ test "ShiftLeft down from NoModifier" <|
                \_ ->
                    Layout.keyModDown "ShiftLeft" NoModifier
                        |> Expect.equal Shift
            , test "ShiftRight down from NoModifier" <|
                \_ ->
                    Layout.keyModDown "ShiftRight" NoModifier
                        |> Expect.equal Shift
            , test "non-shift key does not change modifier" <|
                \_ ->
                    Layout.keyModDown "KeyA" NoModifier
                        |> Expect.equal NoModifier
            , test "ShiftLeft up from Shift" <|
                \_ ->
                    Layout.keyModUp "ShiftLeft" Shift
                        |> Expect.equal NoModifier
            , test "CapsLock up toggles" <|
                \_ ->
                    Layout.keyModUp "CapsLock" NoModifier
                        |> Expect.equal CapsLock
            , test "CapsLock up toggles back" <|
                \_ ->
                    Layout.keyModUp "CapsLock" CapsLock
                        |> Expect.equal NoModifier
            ]
        , describe "modifierToString"
            [ test "NoModifier -> Nothing" <|
                \_ ->
                    Layout.modifierToString NoModifier
                        |> Expect.equal Nothing
            , test "Shift -> Just Shift" <|
                \_ ->
                    Layout.modifierToString Shift
                        |> Expect.equal (Just "Shift")
            , test "CapsLock -> Just CapsLock" <|
                \_ ->
                    Layout.modifierToString CapsLock
                        |> Expect.equal (Just "CapsLock")
            , test "ShiftCapsLock -> Just Shift + CapsLock" <|
                \_ ->
                    Layout.modifierToString ShiftCapsLock
                        |> Expect.equal (Just "Shift + CapsLock")
            ]
        , describe "initLayout"
            [ test "can init SilPowerG" <|
                \_ ->
                    case Layout.initLayout Layout.SilPowerG of
                        Layout.PowerGLayout _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected PowerGLayout"
            , test "can init PowerGeez" <|
                \_ ->
                    case Layout.initLayout Layout.PowerGeez of
                        Layout.PowerGeezLayout _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected PowerGeezLayout"
            , test "can init GeezIME" <|
                \_ ->
                    case Layout.initLayout Layout.GeezIME of
                        Layout.GeezIMELayout _ ->
                            Expect.pass

                        _ ->
                            Expect.fail "Expected GeezIMELayout"
            ]
        , describe "codePoints"
            [ test "has 34 entries (one per physical key)" <|
                \_ ->
                    Layout.codePoints
                        |> List.length
                        |> Expect.equal 34
            , test "no duplicates" <|
                \_ ->
                    let
                        sorted =
                            List.sort Layout.codePoints

                        unique =
                            dedup sorted
                    in
                    List.length sorted
                        |> Expect.equal (List.length unique)
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
