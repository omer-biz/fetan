module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown, onKeyUp)
import Dict exposing (Dict)
import Html exposing (Html, div, main_, text)
import Html.Attributes exposing (class, tabindex)
import Html.Events exposing (onBlur, onFocus, preventDefaultOn)
import Json.Decode as Decode
import Layout


type alias Model =
    { focusKeyBr : Bool
    , isShiftPressed : Bool
    , keys : Array (Array Key)
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


specialKeys : Dict String String
specialKeys =
    Dict.fromList
        [ ( "Tab", "flex-grow" )
        , ( "CapsLock", "flex-grow" )
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


createArray : List ( String, String, String ) -> Array Key
createArray keyList =
    keyList
        |> List.map toKey
        |> Array.fromList


layoutToArray : List (List ( String, String, String )) -> Array (Array Key)
layoutToArray layout =
    layout
        |> List.map createArray
        |> Array.fromList


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FocusKeyBr ->
            ( { model | focusKeyBr = True }, Cmd.none )

        BlurKeyBr ->
            ( { model | focusKeyBr = False }, Cmd.none )

        KeyDown key ->
            let
                updated =
                    updateKeyStatus Pressed key model.keys

                shiftDown =
                    if key == "ShiftLeft" || key == "ShiftRight" then
                        True

                    else
                        model.isShiftPressed
            in
            ( { model | keys = updated, isShiftPressed = shiftDown }, Cmd.none )

        KeyUp key ->
            let
                updated =
                    updateKeyStatus Released key model.keys

                shiftUp =
                    if key == "ShiftLeft" || key == "ShiftRight" then
                        False

                    else
                        model.isShiftPressed
            in
            ( { model | keys = updated, isShiftPressed = shiftUp }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


updateKeyStatus : KeyStatus -> String -> Array (Array Key) -> Array (Array Key)
updateKeyStatus s key keys =
    let
        findKey k =
            if k.code == key then
                { k | status = s }

            else
                k

        modKey row =
            Array.map findKey row
    in
    Array.map modKey keys


view : Model -> Html Msg
view model =
    main_
        [ class "text-white flex items-center justify-center h-screen"
        ]
        [ viewKeyBoard model ]


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
        ] (model.keys
            |> Array.map (viewRow model.isShiftPressed)
            |> Array.toList
        )


viewRow : Bool -> Array Key -> Html msg
viewRow shiftOn row =
    div [ class "flex justify-center gap-1 py-1" ]
        (row
            |> Array.map (viewKey shiftOn)
            |> Array.toList
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
        [ text keyView ]


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
    Decode.map (\a -> a)
        (Decode.field "code" Decode.string)


init : () -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model False False <| layoutToArray Layout.silPowerG
    in
    ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
