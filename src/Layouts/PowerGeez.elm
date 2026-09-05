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
            -- let
            --     _ =
            --         Debug.log "he" <| Char.toCode p
            -- in
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


findKeyForChar : Char -> Maybe ( KeyModifier, String )
findKeyForChar c =
    let
        findIn dict mod =
            Dict.toList dict
                |> List.filter (\( _, v ) -> v == c)
                |> List.head
                |> Maybe.map (\( k, _ ) -> ( mod, k ))
    in
    findIn plainKeys NoModifier
        |> orElse (findIn shiftKeys Shift)
        |> orElse (findIn capsKeys CapsLock)
        |> orElse (findIn shiftCaps ShiftCapsLock)


orElse : Maybe a -> Maybe a -> Maybe a
orElse fallback maybe =
    case maybe of
        Just val ->
            Just val

        Nothing ->
            fallback


normalizeLetter : Char -> ( Char, Maybe Char )
normalizeLetter letter =
    let
        cl =
            Char.toCode letter

        vowelOffset =
            modBy 0x08 <| modBy 0x10 cl

        vowelPart =
            if vowelOffset > 0 && vowelOffset < 8 then
                Just <| Char.fromCode (0x12A0 + vowelOffset)

            else
                Nothing

        baseCode =
            if modBy 0x10 cl >= 8 then
                ((cl // 0x10) * 0x10) + 8

            else
                (cl // 0x10) * 0x10
    in
    ( Char.fromCode baseCode, vowelPart )


hint : Char -> Model -> Maybe ( KeyModifier, String )
hint input model =
    let
        ( baseChar, vowelPart ) =
            normalizeLetter input
    in
    if vowelPart == Nothing then
        findKeyForChar baseChar

    else if model.partial /= Nothing then
        findKeyForChar (Maybe.withDefault '\u{0000}' vowelPart)

    else
        findKeyForChar baseChar


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
        , ( "KeyQ", 'ቐ' )
        , ( "KeyG", 'ጘ' )
        , ( "KeyD", 'ዸ' )
        , ( "KeyW", 'ኧ' )
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
