module Layouts.GeezIME exposing (..)

import Dict
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


type alias Model =
    Maybe Char


empty : Model
empty =
    Nothing


init : Model
init =
    empty


render : KeyModifier -> String -> Model -> String
render state code _ =
    case state of
        NoModifier ->
            Dict.get code plainKeys
                |> getDouble code
                |> Maybe.withDefault ""

        _ ->
            Dict.get code shiftKeys
                |> Maybe.withDefault ' '
                |> String.fromChar


getDouble : String -> Maybe Char -> Maybe String
getDouble code key =
    case key of
        Just k ->
            Just <| String.fromChar k

        Nothing ->
            Dict.get code doublePlain
                |> Maybe.map (Tuple.mapBoth String.fromChar String.fromChar)
                |> Maybe.map (\( a, b ) -> a ++ b)


getKey : KeyModifier -> String -> String
getKey state code =
    case state of
        NoModifier ->
            Dict.get code plainKeys
                |> Maybe.withDefault ' '
                |> String.fromChar

        _ ->
            Dict.get code shiftKeys
                |> Maybe.withDefault ' '
                |> String.fromChar


update : KeyModifier -> String -> Char -> Model -> ( Model, KeyAttempt )
update state code curr model =
    let
        newModel =
            keyFromBoard state code model
    in
    checkWithCurr curr newModel


checkWithCurr : Char -> Maybe Char -> ( Model, KeyAttempt )
checkWithCurr curr model =
    case model of
        Nothing ->
            ( empty, Wrong )

        Just head ->
            if head == curr then
                ( empty, Correct )

            else if isPartial head curr then
                ( model, Partial )

            else
                ( empty, Wrong )


isPartial : Char -> Char -> Bool
isPartial prev curr =
    let
        isSec =
            modBy 0x10 (Char.toCode curr) == 0x0A || modBy 0x10 (Char.toCode curr) == 0x02
    in
    (isTheSameFamily prev curr
        && canAcceptVowel prev
        && ((not << canAcceptVowel) curr || isSec)
    )
        || checkDoubleKey prev curr


checkDoubleKey prev curr =
    let
        prevUni =
            Char.toCode prev

        currUni =
            Char.toCode curr

        ( currB, _ ) =
            boardKey curr
    in
    Dict.values doublePlain
        |> List.map (\( fst, sec ) -> prev == fst && currB == sec)
        |> List.filter ((==) True)
        |> (not << List.isEmpty)


isTheSameFamily prev curr =
    let
        prevUni =
            Char.toCode prev

        currUni =
            Char.toCode curr
    in
    -- are in the same block
    (prevUni // 0x10 == currUni // 0x10)
        -- are in the upper block
        && ((modBy 0x10 prevUni > 0x07 && modBy 0x10 currUni > 0x07)
                -- are in the lower block
                || (modBy 0x10 prevUni <= 0x07 && modBy 0x10 currUni <= 0x07)
           )


keyFromBoard : KeyModifier -> String -> Maybe Char -> Maybe Char
keyFromBoard state code context =
    let
        doubleKeys =
            Dict.map (\_ v -> Tuple.first v) doublePlain

        keys =
            if state == NoModifier then
                keysFromState state
                    |> Dict.union doubleKeys

            else
                keysFromState state
    in
    case context of
        Nothing ->
            Dict.get code keys

        Just head ->
            Just <| modKey head code


modKey : Char -> String -> Char
modKey prev code =
    let
        dobFst =
            Dict.values doublePlain
                |> List.map Tuple.first

        vKeys =
            Dict.keys vowelOffsets
    in
    if List.member prev dobFst && (not <| List.member code vKeys) then
        Dict.get code doublePlain
            |> Maybe.map Tuple.second
            |> Maybe.withDefault ' '

    else if List.member code vKeys && canAcceptVowel prev then
        withVowel prev code

    else
        ' '


canAcceptVowel : Char -> Bool
canAcceptVowel letter =
    let
        c =
            letter
                |> Char.toCode
                |> modBy 0x10
    in
    c == 0x05 || c == 0x0D || c == 0x02


withVowel : Char -> String -> Char
withVowel prev code =
    let
        unicode =
            Char.toCode prev

        tail =
            modBy 0x10 unicode
    in
    if tail == 0x05 || tail == 0x0D then
        Dict.get code vowelOffsets
            |> Maybe.map ((+) unicode >> Char.fromCode)
            |> Maybe.withDefault ' '

    else
        Char.fromCode (unicode + 2)


vowelOffsets : Dict.Dict String number
vowelOffsets =
    Dict.fromList
        [ ( "KeyE", -5 ), ( "KeyU", -4 ), ( "KeyI", -3 ), ( "KeyA", -2 ), ( "KeyO", 1 ) ]


keysFromState state =
    case state of
        NoModifier ->
            plainKeys

        _ ->
            shiftKeys


boardKey : Char -> ( Char, String )
boardKey key =
    let
        keyCode =
            Char.toCode key

        lstDgt =
            modBy 0x10 keyCode

        conv =
            (keyCode // 0x10) * 0x10 + 0x05

        helper =
            if lstDgt >= 0x08 then
                conv + 0x08

            else
                conv
    in
    ( Char.fromCode helper, "" )


hint : Char -> Model -> Maybe ( KeyModifier, String )
hint _ _ =
    Nothing


plainKeys : Dict.Dict String Char
plainKeys =
    Dict.fromList
        [ ( "KeyL", 'ል' )
        , ( "KeyM", 'ም' )
        , ( "KeyR", 'ር' )
        , ( "KeyQ", 'ቅ' )
        , ( "KeyB", 'ብ' )
        , ( "KeyV", 'ቭ' )
        , ( "KeyT", 'ት' )
        , ( "KeyC", 'ች' )
        , ( "KeyN", 'ን' )
        , ( "KeyE", 'አ' )
        , ( "KeyU", 'ኡ' )
        , ( "KeyI", 'ኢ' )
        , ( "KeyA", 'ኣ' )
        , ( "KeyO", 'ኦ' )
        , ( "KeyK", 'ክ' )
        , ( "KeyW", 'ው' )
        , ( "KeyZ", 'ዝ' )
        , ( "KeyY", 'ይ' )
        , ( "KeyD", 'ድ' )
        , ( "KeyJ", 'ጅ' )
        , ( "KeyG", 'ግ' )
        , ( "KeyF", 'ፍ' )
        , ( "KeyP", 'ፕ' )
        , ( "Space", ' ' )
        ]


shiftKeys : Dict.Dict String Char
shiftKeys =
    Dict.fromList
        [ ( "KeyH", 'ሕ' )
        , ( "KeyO", 'ዕ' )
        , ( "KeyZ", 'ዥ' )
        , ( "KeyC", 'ጭ' )
        , ( "KeyP", 'ጵ' )
        , ( "KeyT", 'ጥ' )
        , ( "KeyK", 'ኽ' )
        , ( "KeyS", 'ሽ' )
        ]


doublePlain : Dict.Dict String ( Char, Char )
doublePlain =
    Dict.fromList
        [ ( "KeyS", ( 'ስ', 'ሥ' ) )
        , ( "KeyH", ( 'ህ', 'ኅ' ) )
        , ( "KeyX", ( 'ጽ', 'ፅ' ) )
        ]
