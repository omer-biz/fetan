module GeezIMETest exposing (..)

import Expect
import Layouts.GeezIME as GeezIME
import Test exposing (..)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


suite : Test
suite =
    describe "Layouts.GeezIME"
        [ describe "update - single-keystroke letters"
            [ test "pressing 'e' when target is 'አ' -> Correct" <|
                \_ ->
                    let
                        ( _, result ) =
                            GeezIME.update NoModifier "KeyE" 'አ' GeezIME.init
                    in
                    Expect.equal Correct result
            , test "pressing 'a' when target is 'ኣ' -> Correct" <|
                \_ ->
                    let
                        ( _, result ) =
                            GeezIME.update NoModifier "KeyA" 'ኣ' GeezIME.init
                    in
                    Expect.equal Correct result
            , test "pressing wrong key 'b' when target is 'አ' -> Wrong" <|
                \_ ->
                    let
                        ( _, result ) =
                            GeezIME.update NoModifier "KeyB" 'አ' GeezIME.init
                    in
                    Expect.equal Wrong result
            ]
        , describe "update - multi-keystroke letters"
            [ test "'m' when target is 'መ' -> Partial (need 'me')" <|
                \_ ->
                    let
                        ( _, result ) =
                            GeezIME.update NoModifier "KeyM" 'መ' GeezIME.init
                    in
                    Expect.equal Partial result
            , test "'m' then 'e' when target is 'መ' -> Correct" <|
                \_ ->
                    let
                        ( model1, _ ) =
                            GeezIME.update NoModifier "KeyM" 'መ' GeezIME.init

                        ( _, result ) =
                            GeezIME.update NoModifier "KeyE" 'መ' model1
                    in
                    Expect.equal Correct result
            , test "'m' then 'a' when target is 'ማ' -> Correct" <|
                \_ ->
                    let
                        ( model1, _ ) =
                            GeezIME.update NoModifier "KeyM" 'ማ' GeezIME.init

                        ( _, result ) =
                            GeezIME.update NoModifier "KeyA" 'ማ' model1
                    in
                    Expect.equal Correct result
            , test "'m' when target is 'ማ' -> Partial" <|
                \_ ->
                    let
                        ( _, result ) =
                            GeezIME.update NoModifier "KeyM" 'ማ' GeezIME.init
                    in
                    Expect.equal Partial result
            , test "'b' then 'u' when target is 'ቡ' -> Correct" <|
                \_ ->
                    let
                        ( model1, _ ) =
                            GeezIME.update NoModifier "KeyB" 'ቡ' GeezIME.init

                        ( _, result ) =
                            GeezIME.update NoModifier "KeyU" 'ቡ' model1
                    in
                    Expect.equal Correct result
            ]
        , describe "update - shifted letters"
            [ test "Shift + 'h' when target is 'ሐ' -> Partial (need 'He')" <|
                \_ ->
                    let
                        ( _, result ) =
                            GeezIME.update Shift "KeyH" 'ሐ' GeezIME.init
                    in
                    Expect.equal Partial result
            , test "Shift+'h' then 'e' when target is 'ሐ' -> Correct" <|
                \_ ->
                    let
                        ( model1, _ ) =
                            GeezIME.update Shift "KeyH" 'ሐ' GeezIME.init

                        ( _, result ) =
                            GeezIME.update NoModifier "KeyE" 'ሐ' model1
                    in
                    Expect.equal Correct result
            ]
        , describe "update - model resets after Correct"
            [ test "model sequence resets to empty after correct match" <|
                \_ ->
                    let
                        ( model1, _ ) =
                            GeezIME.update NoModifier "KeyM" 'መ' GeezIME.init

                        ( model2, _ ) =
                            GeezIME.update NoModifier "KeyE" 'መ' model1
                    in
                    Expect.equal GeezIME.empty model2
            , test "model sequence resets to empty after wrong key" <|
                \_ ->
                    let
                        ( model1, _ ) =
                            GeezIME.update NoModifier "KeyZ" 'መ' GeezIME.init
                    in
                    Expect.equal GeezIME.empty model1
            ]
        , describe "hint"
            [ test "hint for 'መ' with empty sequence -> Just (NoModifier, KeyM)" <|
                \_ ->
                    GeezIME.hint 'መ' GeezIME.init
                        |> Expect.equal (Just ( NoModifier, "KeyM" ))
            , test "hint for 'ማ' with empty sequence -> Just (NoModifier, KeyM)" <|
                \_ ->
                    GeezIME.hint 'ማ' GeezIME.init
                        |> Expect.equal (Just ( NoModifier, "KeyM" ))
            , test "hint for 'ማ' after typing 'm' -> Just (NoModifier, KeyA)" <|
                \_ ->
                    let
                        ( model1, _ ) =
                            GeezIME.update NoModifier "KeyM" 'ማ' GeezIME.init
                    in
                    GeezIME.hint 'ማ' model1
                        |> Expect.equal (Just ( NoModifier, "KeyA" ))
            , test "hint for 'አ' with empty sequence -> Just (NoModifier, KeyE)" <|
                \_ ->
                    GeezIME.hint 'አ' GeezIME.init
                        |> Expect.equal (Just ( NoModifier, "KeyE" ))
            ]
        , describe "render"
            [ test "renders Ethiopic character for KeyM with NoModifier" <|
                \_ ->
                    let
                        rendered =
                            GeezIME.render NoModifier "KeyM" GeezIME.init
                    in
                    -- render returns the Ethiopic character mapped to this key
                    (String.length rendered > 0)
                        |> Expect.equal True
            , test "renders different character for KeyM with Shift vs NoModifier" <|
                \_ ->
                    let
                        normal =
                            GeezIME.render NoModifier "KeyM" GeezIME.init

                        shifted =
                            GeezIME.render Shift "KeyM" GeezIME.init
                    in
                    Expect.notEqual normal shifted
            ]
        ]
