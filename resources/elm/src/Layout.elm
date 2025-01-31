module Layout exposing (Judge, KeyAttempt(..), KeyModifier(..), Layout, codePoints, keyModDown, keyModUp)


type KeyModifier
    = NoModifier
    | CapsLock
    | Shift
    | ShiftCapsLock


type KeyAttempt state
    = Wrong
    | Correct
    | Partial state


type alias Judge state =
    KeyModifier -> String -> Char -> state -> KeyAttempt state


type alias Printer =
    KeyModifier -> String -> String


type alias Layout state =
    { printer : Printer
    , judge : Judge state
    , state : state
    }


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
    [ "Backquote"
    , "Digit1"
    , "Digit2"
    , "Digit3"
    , "Digit4"
    , "Digit5"
    , "Digit6"
    , "Digit7"
    , "Digit8"
    , "Digit9"
    , "Digit0"
    , "Minus"
    , "Equal"
    , "KeyQ"
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
