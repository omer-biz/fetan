module Main exposing (main)

import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict exposing (Dict, update)
import Dictation as DictGen
import Html exposing (Html, div, main_, p, span, text)
import Html.Attributes exposing (class, tabindex)
import Html.Events exposing (onBlur, onFocus, preventDefaultOn)
import Json.Decode as Decode
import Json.Encode exposing (dict)
import Layout
import Random


type alias Model =
    { focusKeyBr : Bool
    , isShiftPressed : Bool
    , keys : List (List Key)
    , dictation : Dictation
    }


type alias Dictation =
    { prev : List Letter
    , current : Letter
    , next : List Letter
    , try : Maybe Char
    , done : Bool
    }


type LetterState
    = New
    | Wrong
    | Rolling
    | RollingW -- stained rolling


type alias Letter =
    { letter : Char
    , state : LetterState
    }


type KeyStatus
    = Pressed
    | Released


type alias Key =
    { view : String
    , altView : String
    , code : String
    , status : KeyStatus
    , form : KeyType
    }


type KeyType
    = Normal
    | Special { extraStyle : String }


type Msg
    = NoOp
    | KeyDown String
    | KeyUp String
    | FocusKeyBr
    | BlurKeyBr
    | NewDict String


stringToDictation : String -> Dictation
stringToDictation str =
    case String.uncons str of
        Just ( curr, next ) ->
            Dictation (lettersFromString "") (Letter curr New) (lettersFromString next) Nothing False

        Nothing ->
            Dictation (lettersFromString "?") (Letter '?' New) (lettersFromString "?") Nothing False


lettersFromString : String -> List Letter
lettersFromString str =
    str
        |> String.toList
        |> List.map (\l -> Letter l New)


specialKeys : Dict String String
specialKeys =
    Dict.fromList
        [ ( "Tab", "flex-grow" )
        , ( "CapsLock", "w-24" )
        , ( "ShiftLeft", "flex-grow" )
        , ( "ShiftRight", "flex-grow" )
        , ( "ControlLeft", "w-20" )
        , ( "ControlRight", "w-20" )
        , ( "ALT", "w-20" )
        , ( "AltLeft", "w-20" )
        , ( "AltRight", "w-20" )
        , ( "Space", "flex-grow" )
        , ( "Enter", "flex-grow" )
        , ( "Backspace", "flex-grow w-24" )
        , ( "Backslash", "flex-grow" )
        ]


toKey : ( String, String, String ) -> Key
toKey ( v, a, c ) =
    specialKeys
        |> Dict.get c
        |> Maybe.andThen (\s -> Just (Key v a c Released <| Special { extraStyle = s }))
        |> Maybe.withDefault (Key v a c Released Normal)


createList : List ( String, String, String ) -> List Key
createList keyList =
    keyList
        |> List.map toKey


layoutToList : List (List ( String, String, String )) -> List (List Key)
layoutToList layout =
    layout
        |> List.map createList


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    let
        helper remainingList =
            case remainingList of
                [] ->
                    Nothing

                x :: xs ->
                    if predicate x then
                        Just x

                    else
                        helper xs
    in
    helper list


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusKeyBr ->
            ( { model | focusKeyBr = True }, Cmd.none )

        BlurKeyBr ->
            ( { model | focusKeyBr = False }, Cmd.none )

        KeyDown key ->
            let
                updatedKeys =
                    updateKeyStatus Pressed key model.keys

                shiftDown =
                    if key == "ShiftLeft" || key == "ShiftRight" then
                        True

                    else
                        model.isShiftPressed
            in
            ( { model
                | keys = updatedKeys
                , isShiftPressed = shiftDown
              }
            , Cmd.none
            )

        KeyUp key ->
            let
                updated =
                    updateKeyStatus Released key model.keys

                shiftUp =
                    if key == "ShiftLeft" || key == "ShiftRight" then
                        False

                    else
                        model.isShiftPressed

                altViewOrView : Key -> Bool
                altViewOrView k =
                    k.code == key

                extractLetter k =
                    if model.isShiftPressed then
                        k.altView

                    else
                        k.view

                acctualLetter =
                    model.keys
                        |> List.concat
                        |> find altViewOrView
                        |> Maybe.map extractLetter

                updatedDict =
                    case acctualLetter of
                        Just tryKey ->
                            case String.uncons tryKey of
                                Just ( tk, "" ) ->
                                    updateDictState tk model.dictation

                                _ ->
                                    model.dictation

                        _ ->
                            model.dictation
            in
            ( { model
                | keys = updated
                , isShiftPressed = shiftUp
                , dictation = updatedDict
              }
            , Cmd.none
            )

        NewDict dict ->
            ( { model | dictation = stringToDictation dict }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateDictState : Char -> Dictation -> Dictation
updateDictState tryKey dict =
    let
        current =
            dict.current

        toNextChar =
            case dict.next of
                newCurr :: next ->
                    { dict
                        | next = next
                        , current = newCurr
                        , prev = dict.prev ++ [ current ]
                    }

                [] ->
                    { dict | done = True }

        tkUnicode =
            Char.toCode tryKey

        clUnicode =
            Char.toCode current.letter

        wrongAttempt st =
            { dict | try = Nothing, current = { current | state = st } }

        stateUp oSt =
            if oSt == New then
                Rolling

            else
                RollingW
    in
    if tryKey == current.letter then
        toNextChar

    else if tkUnicode >= 0x12A1 && tkUnicode <= 0x12A7 then
        -- if it's a vowel
        case dict.try of
            Just incomplete ->
                if Char.toCode incomplete + (tkUnicode - 0x12A0) == clUnicode then
                    toNextChar

                else
                    wrongAttempt RollingW

            Nothing ->
                wrongAttempt Wrong

    else if (clUnicode - tkUnicode) > 0 && (clUnicode - tkUnicode) <= 7 then
        { dict
            | try = Just tryKey
            , current = { current | state = stateUp current.state }
        }

    else
        wrongAttempt Wrong


updateKeyStatus : KeyStatus -> String -> List (List Key) -> List (List Key)
updateKeyStatus s key keys =
    let
        findKey k =
            if k.code == key then
                { k | status = s }

            else
                k

        modKey row =
            List.map findKey row
    in
    List.map modKey keys


view : Model -> Html Msg
view model =
    main_ [ class "text-white flex items-center justify-center h-screen flex-col" ]
        [ div [] [ viewDictation model.dictation, viewKeyBoard model ]
        ]


viewDictation : Dictation -> Html msg
viewDictation dict =
    let
        currentKeyStyle =
            case dict.current.state of
                Rolling ->
                    "text-yellow-300"

                Wrong ->
                    "text-red-400"

                _ ->
                    ""

        viewLetter lt =
            let
                styleDot =
                    if lt.letter == '.' then
                        " text-4xl font-bold text-gray-300"

                    else
                        ""

                styleState =
                    case lt.state of
                        Rolling ->
                            if lt.letter == dict.current.letter then
                                "text-yellow-300"

                            else
                                "text-gray-400"

                        New ->
                            " "

                        Wrong ->
                            "text-red-400"

                        RollingW ->
                            if lt.letter == dict.current.letter then
                                "text-yellow-300"

                            else
                                "text-red-400"
            in
            span [ class <| String.join " " [ styleDot, styleState ] ]
                [ text <| (lt.letter |> String.fromChar |> String.replace " " " · ") ]

        viewLetters lts =
            List.map viewLetter lts
    in
    div [ class "mx-auto border rounded border-2 border-white p-4 mb-4 max-w-[800px] text-3xl font-normal leading-relaxed" ]
        [ p [ class "inline m-0 p-0 text-gray-400" ] (viewLetters dict.prev)
        , p [ class <| String.join " " [ "underline inline m-0 p-0", currentKeyStyle ] ]
            [ viewLetter dict.current ]
        , p [ class "inline m-0 p-0" ] (viewLetters dict.next)
        ]


keyDown : msg -> Html.Attribute msg
keyDown msg =
    preventDefaultOn "keydown" <|
        Decode.map (\a -> ( a, True )) (Decode.succeed msg)


keyUp : msg -> Html.Attribute msg
keyUp msg =
    preventDefaultOn "keyup" <|
        Decode.map (\a -> ( a, True )) (Decode.succeed msg)


viewKeyBoard : Model -> Html Msg
viewKeyBoard model =
    let
        isfocused =
            if model.focusKeyBr then
                "border-4 border-gray-600"

            else
                ""
    in
    div
        [ class <| "border-2 p-10 rounded border-gray-300 " ++ isfocused
        , onFocus FocusKeyBr
        , onBlur BlurKeyBr
        , tabindex 0 -- Helps make a div focusable and blurable.
        , keyDown NoOp
        , keyUp NoOp
        ]
        (model.keys
            |> List.map (viewRow model.isShiftPressed)
        )


viewRow : Bool -> List Key -> Html msg
viewRow shiftOn row =
    div [ class "flex justify-center gap-1 py-1" ]
        (row
            |> List.map (viewKey shiftOn)
        )


viewKey : Bool -> Key -> Html msg
viewKey shiftOn key =
    let
        isPressed =
            case key.status of
                Released ->
                    "bg-gray-600"

                Pressed ->
                    "bg-lime-500"

        keyWidth =
            case key.form of
                Normal ->
                    "w-12"

                Special f ->
                    f.extraStyle

        keyView =
            if shiftOn then
                key.altView

            else
                key.view
    in
    div
        [ class <| "px-4 py-2 text-white text-center rounded shadow font-semibold " ++ isPressed ++ " " ++ keyWidth ]
        [ text keyView
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.focusKeyBr then
        Sub.batch
            [ onKeyDown <| Decode.map KeyDown keyDecoder
            , onKeyUp <| Decode.map KeyUp keyDecoder
            ]

    else
        Sub.none


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.map (\a -> a) <|
        Decode.field "code" Decode.string


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model False False (layoutToList Layout.silPowerG) (stringToDictation "")
    in
    ( model, Random.generate NewDict <| DictGen.genAll 20 )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
