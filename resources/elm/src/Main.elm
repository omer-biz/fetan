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
    , keys : Array (Array Key)
    }


type KeyStatus
    = Pressed
    | Released


type alias Key =
    { view : String
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


toKeyT : ( String, String ) -> Key
toKeyT ( v, c ) =
    specialKeys
        |> Dict.get c
        |> Maybe.andThen (\s -> Just (Key v c Released <| Special { extraStyle = s }))
        |> Maybe.withDefault (Key v c Released Normal)


createArray : List ( String, String ) -> Array Key
createArray keyList =
    keyList
        |> List.map toKeyT
        |> Array.fromList


layoutToArray : List (List ( String, String )) -> Array (Array Key)
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
            in
            ( { model | keys = updated }, Cmd.none )

        KeyUp key ->
            let
                updated =
                    updateKeyStatus Released key model.keys
            in
            ( { model | keys = updated }, Cmd.none )

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
        [ viewKeyBoard model.keys model.focusKeyBr ]


keyDown : msg -> Html.Attribute msg
keyDown msg =
    preventDefaultOn "keydown" (Decode.map alwaysPreventDefault (Decode.succeed msg))


keyUp : msg -> Html.Attribute msg
keyUp msg =
    preventDefaultOn "keyup" (Decode.map alwaysPreventDefault (Decode.succeed msg))


alwaysPreventDefault : msg -> ( msg, Bool )
alwaysPreventDefault msg =
    ( msg, True )


viewKeyBoard : Array (Array Key) -> Bool -> Html Msg
viewKeyBoard keys foc =
    let
        isfocused =
            if foc then
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
        (keys
            |> Array.map viewRow
            |> Array.toList
        )


viewRow : Array Key -> Html msg
viewRow row =
    div [ class "flex justify-center gap-1 py-1" ]
        (row
            |> Array.map viewKey
            |> Array.toList
        )


viewKey : Key -> Html msg
viewKey key =
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
    in
    div
        [ class <| "px-4 py-2 text-white text-center rounded shadow " ++ isPressed ++ " " ++ keyWidth
        ]
        [ text key.view ]


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
            Model False <| layoutToArray Layout.qwerty
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
