module Models.Layout exposing (..)

import Layouts.GeezIME as GeezIME
import Layouts.PowerGeez as PowerGeez
import Layouts.SilPowerG as SilPowerG
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


type Layout
    = PowerGLayout SilPowerG.Model
    | PowerGeezLayout PowerGeez.Model
    | GeezIMELayout GeezIME.Model


init : Layout
init =
    PowerGLayout SilPowerG.init



update : KeyModifier -> String -> Char -> Layout -> ( Layout, KeyAttempt )
update keybrState codePoint currentLetter layout =
    case layout of
        PowerGLayout model ->
            let
                ( newModel, result ) =
                    SilPowerG.update keybrState codePoint currentLetter model
            in
            ( PowerGLayout newModel, result )

        PowerGeezLayout model ->
            let
                ( newModel, result ) =
                    PowerGeez.update keybrState codePoint currentLetter model
            in
            ( PowerGeezLayout newModel, result )

        GeezIMELayout model ->
            let
                ( newModel, result ) =
                    GeezIME.update keybrState codePoint currentLetter model
            in
            ( GeezIMELayout newModel, result )


render : KeyModifier -> String -> Layout -> String
render keybrState codePoint layout =
    case layout of
        PowerGLayout model ->
            SilPowerG.render keybrState codePoint model

        PowerGeezLayout model ->
            PowerGeez.render keybrState codePoint model

        GeezIMELayout model ->
            GeezIME.render keybrState codePoint model


hint : Char -> Layout -> Maybe ( KeyModifier, String )
hint curr layout =
    case layout of
        PowerGLayout model ->
            SilPowerG.hint curr model

        PowerGeezLayout model ->
            PowerGeez.hint curr model

        GeezIMELayout model ->
            GeezIME.hint curr model



-- Helpers


modifierToString : KeyModifier -> Maybe String
modifierToString modifier =
    case modifier of
        CapsLock ->
            Just "CapsLock"

        Shift ->
            Just "Shift"

        ShiftCapsLock ->
            Just "Shift + CapsLock"

        NoModifier ->
            Nothing


shiftDown : KeyModifier -> KeyModifier
shiftDown mod =
    case mod of
        CapsLock ->
            ShiftCapsLock

        NoModifier ->
            Shift

        _ ->
            mod


shiftUp : KeyModifier -> KeyModifier
shiftUp mod =
    case mod of
        ShiftCapsLock ->
            CapsLock

        Shift ->
            NoModifier

        _ ->
            mod


capsFlip : KeyModifier -> KeyModifier
capsFlip mod =
    case mod of
        ShiftCapsLock ->
            Shift

        NoModifier ->
            CapsLock

        CapsLock ->
            NoModifier

        Shift ->
            ShiftCapsLock


keyModDown : String -> KeyModifier -> KeyModifier
keyModDown key modifier =
    if key == "ShiftLeft" || key == "ShiftRight" then
        shiftDown modifier

    else
        modifier


keyModUp : String -> KeyModifier -> KeyModifier
keyModUp key modifier =
    if key == "ShiftLeft" || key == "ShiftRight" then
        shiftUp modifier

    else if key == "CapsLock" then
        capsFlip modifier

    else
        modifier


codePoints : List String
codePoints =
    [ "KeyQ"
    , "KeyW"
    , "KeyE"
    , "KeyR"
    , "KeyT"
    , "KeyY"
    , "KeyU"
    , "KeyI"
    , "KeyO"
    , "KeyP"
    , "BracketLeft"
    , "BracketRight"
    , "Backslash"
    , "KeyA"
    , "KeyS"
    , "KeyD"
    , "KeyF"
    , "KeyG"
    , "KeyH"
    , "KeyJ"
    , "KeyK"
    , "KeyL"
    , "Semicolon"
    , "Quote"
    , "KeyZ"
    , "KeyX"
    , "KeyC"
    , "KeyV"
    , "KeyB"
    , "KeyN"
    , "KeyM"
    , "Comma"
    , "Period"
    , "Slash"
    ]
