module Layouts.PowerGeez exposing (Model, hint, init, render, update)

import Dict
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


type alias Model =
    { partial : Maybe Char
    , keyw : Bool
    }


init : Model
init =
    Model Nothing False


render : KeyModifier -> String -> Model -> String
render keybrState codePoint _ =
    helper keybrState codePoint
        |> String.fromChar


update : KeyModifier -> String -> Char -> Model -> ( Model, KeyAttempt )
update keybrState codePoint currentLetter model =
    let
        attempt =
            helper keybrState codePoint

        clUnicode =
            Char.toCode currentLetter

        attemptUnicode =
            Char.toCode attempt

        checkPartial p =
            if Char.toCode p + (attemptUnicode - 0x12A0) == clUnicode then
                Correct

            else
                Wrong

        capsIsOn =
            keybrState == CapsLock || keybrState == ShiftCapsLock

        packInfo p =
            let
                _ =
                    Debug.log "he" <| Char.toCode p
            in
            if Char.toCode p < 0x137D then
                Partial
                -- <| Just (Char.fromCode (Char.toCode p + 0x017D))

            else
                Wrong

        checkComboPartial p =
            if Char.toCode p > 0x137D then
                Correct

            else
                Wrong
    in
    if attempt == currentLetter then
        ( model, Correct )

    else if model.partial /= Nothing && capsIsOn && codePoint == "KeyA" then
        ( model
        , model.partial
            |> Maybe.map checkComboPartial
            |> Maybe.withDefault Wrong
        )

    else if model.partial /= Nothing && capsIsOn && codePoint == "KeyW" then
        ( model
        , model.partial
            |> Maybe.map packInfo
            |> Maybe.withDefault Wrong
        )

    else if attemptUnicode >= 0x12A1 && attemptUnicode <= 0x12A7 then
        ( model
        , model.partial
            |> Maybe.map checkPartial
            |> Maybe.withDefault Wrong
        )

    else if (clUnicode - attemptUnicode) > 0 && (clUnicode - attemptUnicode) <= 7 then
        ( { model | partial = Just attempt }, Partial )

    else
        ( model, Wrong )


hint : Char -> Model -> Maybe ( KeyModifier, String )
hint _ _ =
    Nothing


helper : KeyModifier -> String -> Char
helper keybrState codePoint =
    let
        keys =
            case keybrState of
                NoModifier ->
                    plainKeys

                Shift ->
                    shiftKeys

                ShiftCapsLock ->
                    shiftCaps

                CapsLock ->
                    capsKeys
    in
    keys
        |> Dict.get codePoint
        |> Maybe.withDefault '\u{0000}'


plainKeys : Dict.Dict String Char
plainKeys =
    Dict.fromList
        [ ( "KeyH", 'ሀ' )
        , ( "KeyL", 'ለ' )
        , ( "KeyM", 'መ' )
        , ( "KeyR", 'ረ' )
        , ( "KeyS", 'ሰ' )
        , ( "KeyQ", 'ቀ' )
        , ( "KeyB", 'በ' )
        , ( "KeyV", 'ቨ' )
        , ( "KeyT", 'ተ' )
        , ( "KeyC", 'ቸ' )
        , ( "KeyN", 'ነ' )
        , ( "KeyX", 'አ' )
        , ( "KeyK", 'ከ' )
        , ( "KeyW", 'ወ' )
        , ( "KeyZ", 'ዘ' )
        , ( "KeyD", 'ደ' )
        , ( "KeyJ", 'ጀ' )
        , ( "KeyG", 'ገ' )
        , ( "KeyP", 'ፐ' )
        , ( "KeyU", 'ኡ' )
        , ( "KeyI", 'ኢ' )
        , ( "KeyA", 'ኣ' )
        , ( "KeyY", 'ኤ' )
        , ( "KeyE", 'እ' )
        , ( "KeyO", 'ኦ' )
        , ( "Comma", '፥' )
        , ( "Period", '።' )
        , ( "Slash", '/' )
        , ( "Space", ' ' )
        , ( "KeyF", 'ፈ' )
        , ( "BracketLeft", '[' )
        , ( "BracketRight", ']' )
        , ( "Backslash", '\\' )
        , ( "Semicolon", '፤' )
        , ( "Quote", '\'' )
        ]


shiftKeys : Dict.Dict String Char
shiftKeys =
    Dict.fromList
        [ ( "KeyH", 'ሐ' )
        , ( "KeyS", 'ሠ' )
        , ( "KeyN", 'ኘ' )
        , ( "KeyX", 'ዐ' )
        , ( "KeyZ", 'ዠ' )
        , ( "KeyY", 'የ' )
        , ( "KeyT", 'ጠ' )
        , ( "KeyC", 'ጨ' )
        , ( "KeyP", 'ጰ' )
        , ( "Comma", '<' )
        , ( "Period", '>' )
        , ( "Slash", '?' )
        , ( "BracketLeft", '{' )
        , ( "BracketRight", '}' )
        , ( "Backslash", '|' )
        , ( "Semicolon", '፡' )
        , ( "Quote", '"' )
        ]


capsKeys : Dict.Dict String Char
capsKeys =
    Dict.fromList
        [ ( "KeyS", 'ሸ' )
        , ( "KeyH", 'ኀ' )
        , ( "KeyT", 'ጸ' )
        ]


shiftCaps : Dict.Dict String Char
shiftCaps =
    Dict.fromList
        [ ( "KeyH", 'ኸ' )
        , ( "KeyT", 'ፀ' )
        ]
