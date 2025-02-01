module Layout exposing (Hint, Judge, KeyAttempt(..), KeyModifier(..), Layout, Printer, codePoints, keyModDown, keyModUp, modifierToString)

{-| Represents the different key modifiers that can be applied when typing.
-}


type KeyModifier
    = NoModifier
    | CapsLock
    | Shift
    | ShiftCapsLock


{-| Represents the result of a key attempt, which can be:

  - `Wrong`: The key was incorrect.
  - `Correct`: The key was correct.
  - `Partial state`: The key was partially correct, with a new state to track progress.

-}
type KeyAttempt
    = Wrong
    | Correct
    | Partial (Maybe Char)


{-| A function that judges whether a key attempt is correct, incorrect, or partially correct.
It takes:

  - A `KeyModifier` to check the modifier state.
  - A `String` representing the expected input.
  - A `Char` representing the actual input.
  - A `state` to track progress or context.
    It returns a `KeyAttempt state` indicating the result of the judgment.

-}
type alias Judge =
    KeyModifier -> String -> Char -> Maybe Char -> KeyAttempt


{-| A function that formats or prints the input based on the current key modifier.
It takes:

  - A `KeyModifier` to determine how to format the input.
  - A `String` representing the input to be formatted.
    It returns a `String` representing the formatted output.

-}
type alias Printer =
    KeyModifier -> String -> String


{-| A function that provides a hint for the next key to be pressed.
It takes:

  - A `String` representing the current input
    It returns a tuple containing:
  - A `KeyModifier` suggesting the modifier to use.
  - A `String` suggesting the next key's code point to press.

-}
type alias Hint =
    Char -> Maybe Char -> Maybe ( KeyModifier, String )


{-| Represents a keyboard layout configuration, including:

  - `printer`: A function to format the input.
  - `judge`: A function to judge key attempts.
  - `hint`: An optional function to provide hints.
  - `state`: The current state to track progress or context.

-}
type alias Layout =
    { printer : Printer
    , judge : Judge
    , hint : Maybe Hint

    -- parametrizing this to a state which the layouts could put what ever type
    -- they want is a pain in the proverbial bottom, elm really needs type
    -- classes, Evan please...
    , partial : Maybe Char
    }


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
