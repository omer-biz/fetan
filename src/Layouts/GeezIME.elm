module Layouts.GeezIME exposing (Model, empty, hint, init, render, update)

import Dict exposing (Dict)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))


type alias Model =
    { sequence : String
    }


empty : Model
empty =
    { sequence = "" }


init : Model
init =
    empty


stringToChar : Dict String Char
stringToChar =
    Dict.fromList geezDict


charToString : Dict Char String
charToString =
    List.map (\( k, v ) -> ( v, k )) geezDict |> Dict.fromList


codePointToLatin : KeyModifier -> String -> String
codePointToLatin mod code =
    let
        isShift =
            mod == Shift || mod == ShiftCapsLock || mod == CapsLock
    in
    if String.startsWith "Key" code then
        let
            letter =
                String.dropLeft 3 code |> String.toLower
        in
        if isShift then
            String.toUpper letter

        else
            letter

    else if String.startsWith "Digit" code then
        let
            d =
                String.dropLeft 5 code
        in
        if isShift then
            case d of
                "1" ->
                    "!"

                "2" ->
                    "@"

                "3" ->
                    "#"

                "4" ->
                    "$"

                "5" ->
                    "%"

                "6" ->
                    "^"

                "7" ->
                    "&"

                "8" ->
                    "*"

                "9" ->
                    "("

                "0" ->
                    ")"

                _ ->
                    ""

        else
            d

    else
        case ( code, isShift ) of
            ( "Space", _ ) ->
                " "

            ( "Semicolon", False ) ->
                ";"

            ( "Semicolon", True ) ->
                ":"

            ( "Comma", False ) ->
                ","

            ( "Comma", True ) ->
                "<"

            ( "Period", False ) ->
                "."

            ( "Period", True ) ->
                ">"

            ( "Slash", False ) ->
                "/"

            ( "Slash", True ) ->
                "?"

            ( "Quote", False ) ->
                "'"

            ( "Quote", True ) ->
                "\""

            ( "BracketLeft", False ) ->
                "["

            ( "BracketLeft", True ) ->
                "{"

            ( "BracketRight", False ) ->
                "]"

            ( "BracketRight", True ) ->
                "}"

            ( "Backslash", False ) ->
                "\\"

            ( "Backslash", True ) ->
                "|"

            ( "Minus", False ) ->
                "-"

            ( "Minus", True ) ->
                "_"

            ( "Equal", False ) ->
                "="

            ( "Equal", True ) ->
                "+"

            ( "Backquote", False ) ->
                "`"

            ( "Backquote", True ) ->
                "~"

            _ ->
                ""


render : KeyModifier -> String -> Model -> String
render keybrState codePoint _ =
    let
        mappedChar =
            codePointToLatin keybrState codePoint
    in
    case Dict.get mappedChar stringToChar of
        Just c ->
            String.fromChar c

        Nothing ->
            ""


update : KeyModifier -> String -> Char -> Model -> ( Model, KeyAttempt )
update keybrState codePoint currentLetter model =
    let
        newInput =
            codePointToLatin keybrState codePoint

        newSeq =
            model.sequence ++ newInput

        targetSeq =
            Dict.get currentLetter charToString
                |> Maybe.withDefault (String.fromChar currentLetter)
    in
    if String.isEmpty newInput then
        ( empty, Wrong )

    else if newSeq == targetSeq then
        ( empty, Correct )

    else if String.startsWith newSeq targetSeq then
        ( { sequence = newSeq }, Partial )

    else
        ( empty, Wrong )


latinToCodePoint : String -> Maybe ( KeyModifier, String )
latinToCodePoint str =
    let
        helper : List ( String, ( String, String ) ) -> Maybe ( KeyModifier, String )
        helper list =
            case list of
                [] ->
                    Nothing

                ( code, ( lower, upper ) ) :: rest ->
                    if str == lower then
                        Just ( NoModifier, code )

                    else if str == upper then
                        Just ( Shift, code )

                    else
                        helper rest
    in
    helper usLayout


usLayout : List ( String, ( String, String ) )
usLayout =
    [ ( "KeyA", ( "a", "A" ) )
    , ( "KeyB", ( "b", "B" ) )
    , ( "KeyC", ( "c", "C" ) )
    , ( "KeyD", ( "d", "D" ) )
    , ( "KeyE", ( "e", "E" ) )
    , ( "KeyF", ( "f", "F" ) )
    , ( "KeyG", ( "g", "G" ) )
    , ( "KeyH", ( "h", "H" ) )
    , ( "KeyI", ( "i", "I" ) )
    , ( "KeyJ", ( "j", "J" ) )
    , ( "KeyK", ( "k", "K" ) )
    , ( "KeyL", ( "l", "L" ) )
    , ( "KeyM", ( "m", "M" ) )
    , ( "KeyN", ( "n", "N" ) )
    , ( "KeyO", ( "o", "O" ) )
    , ( "KeyP", ( "p", "P" ) )
    , ( "KeyQ", ( "q", "Q" ) )
    , ( "KeyR", ( "r", "R" ) )
    , ( "KeyS", ( "s", "S" ) )
    , ( "KeyT", ( "t", "T" ) )
    , ( "KeyU", ( "u", "U" ) )
    , ( "KeyV", ( "v", "V" ) )
    , ( "KeyW", ( "w", "W" ) )
    , ( "KeyX", ( "x", "X" ) )
    , ( "KeyY", ( "y", "Y" ) )
    , ( "KeyZ", ( "z", "Z" ) )
    , ( "Digit1", ( "1", "!" ) )
    , ( "Digit2", ( "2", "@" ) )
    , ( "Digit3", ( "3", "#" ) )
    , ( "Digit4", ( "4", "$" ) )
    , ( "Digit5", ( "5", "%" ) )
    , ( "Digit6", ( "6", "^" ) )
    , ( "Digit7", ( "7", "&" ) )
    , ( "Digit8", ( "8", "*" ) )
    , ( "Digit9", ( "9", "(" ) )
    , ( "Digit0", ( "0", ")" ) )
    , ( "Space", ( " ", " " ) )
    , ( "Semicolon", ( ";", ":" ) )
    , ( "Comma", ( ",", "<" ) )
    , ( "Period", ( ".", ">" ) )
    , ( "Slash", ( "/", "?" ) )
    , ( "Quote", ( "'", "\"" ) )
    , ( "BracketLeft", ( "[", "{" ) )
    , ( "BracketRight", ( "]", "}" ) )
    , ( "Backslash", ( "\\", "|" ) )
    , ( "Minus", ( "-", "_" ) )
    , ( "Equal", ( "=", "+" ) )
    , ( "Backquote", ( "`", "~" ) )
    ]


hint : Char -> Model -> Maybe ( KeyModifier, String )
hint targetChar model =
    let
        targetSeq =
            Dict.get targetChar charToString
                |> Maybe.withDefault (String.fromChar targetChar)
    in
    if String.startsWith model.sequence targetSeq then
        let
            remaining =
                String.dropLeft (String.length model.sequence) targetSeq
        in
        remaining
            |> String.uncons
            |> Maybe.andThen (\( nextChar, _ ) -> latinToCodePoint <| String.fromChar nextChar)

    else
        Nothing


geezDict : List ( String, Char )
geezDict =
    [ ( ",", '፣' )
    , ( ".", '።' )
    , ( "..", '.' )
    , ( "...", '፨' )
    , ( "....", '፠' )
    , ( "1^", '፩' )
    , ( "1^0", '፲' )
    , ( "2^", '፪' )
    , ( "2^0", '፳' )
    , ( "3^", '፫' )
    , ( "3^0", '፴' )
    , ( "4^", '፬' )
    , ( "4^0", '፵' )
    , ( "5^", '፭' )
    , ( "5^0", '፶' )
    , ( "6^", '፮' )
    , ( "6^0", '፷' )
    , ( "7^", '፯' )
    , ( "7^0", '፸' )
    , ( "8^", '፰' )
    , ( "8^0", '፹' )
    , ( "9^", '፱' )
    , ( "9^0", '፺' )
    , ( ":", '፥' )
    , ( "::", '፤' )
    , ( ";", '፡' )
    , ( ";-", '፦' )
    , ( "??", '፧' )
    , ( "A", 'እ' )
    , ( "C", 'ጭ' )
    , ( "Ca", 'ጫ' )
    , ( "Ce", 'ጨ' )
    , ( "Ci", 'ጪ' )
    , ( "Cie", 'ጬ' )
    , ( "Co", 'ጮ' )
    , ( "Cu", 'ጩ' )
    , ( "Cua", 'ጯ' )
    , ( "G", 'ጝ' )
    , ( "GW", 'ⶖ' )
    , ( "Ga", 'ጛ' )
    , ( "Ge", 'ጘ' )
    , ( "Gi", 'ጚ' )
    , ( "Gie", 'ጜ' )
    , ( "Go", 'ጞ' )
    , ( "Gu", 'ጙ' )
    , ( "Gua", 'ጟ' )
    , ( "Gue", 'ⶓ' )
    , ( "Gui", 'ⶔ' )
    , ( "Guie", 'ⶕ' )
    , ( "H", 'ሕ' )
    , ( "Ha", 'ሓ' )
    , ( "He", 'ሐ' )
    , ( "Hi", 'ሒ' )
    , ( "Hie", 'ሔ' )
    , ( "Ho", 'ሖ' )
    , ( "Hu", 'ሑ' )
    , ( "Hua", 'ሗ' )
    , ( "K", 'ኽ' )
    , ( "KW", 'ዅ' )
    , ( "Ka", 'ኻ' )
    , ( "Ke", 'ኸ' )
    , ( "Ki", 'ኺ' )
    , ( "Kie", 'ኼ' )
    , ( "Ko", 'ኾ' )
    , ( "Ku", 'ኹ' )
    , ( "Kua", 'ዃ' )
    , ( "Kue", 'ዀ' )
    , ( "Kui", 'ዂ' )
    , ( "Kuie", 'ዄ' )
    , ( "N", 'ኝ' )
    , ( "Na", 'ኛ' )
    , ( "Ne", 'ኘ' )
    , ( "Ni", 'ኚ' )
    , ( "Nie", 'ኜ' )
    , ( "No", 'ኞ' )
    , ( "Nu", 'ኙ' )
    , ( "Nua", 'ኟ' )
    , ( "O", 'ዕ' )
    , ( "Oa", 'ዓ' )
    , ( "Oe", 'ዐ' )
    , ( "Oi", 'ዒ' )
    , ( "Oie", 'ዔ' )
    , ( "Oo", 'ዖ' )
    , ( "Ou", 'ዑ' )
    , ( "P", 'ጵ' )
    , ( "Pa", 'ጳ' )
    , ( "Pe", 'ጰ' )
    , ( "Pi", 'ጲ' )
    , ( "Pie", 'ጴ' )
    , ( "Po", 'ጶ' )
    , ( "Pu", 'ጱ' )
    , ( "Pua", 'ጷ' )
    , ( "Q", 'ቕ' )
    , ( "QW", 'ቝ' )
    , ( "Qa", 'ቓ' )
    , ( "Qe", 'ቐ' )
    , ( "Qi", 'ቒ' )
    , ( "Qie", 'ቔ' )
    , ( "Qo", 'ቖ' )
    , ( "Qu", 'ቑ' )
    , ( "Qua", 'ቛ' )
    , ( "Que", 'ቘ' )
    , ( "Qui", 'ቚ' )
    , ( "Quie", 'ቜ' )
    , ( "S", 'ሽ' )
    , ( "Sa", 'ሻ' )
    , ( "Se", 'ሸ' )
    , ( "Si", 'ሺ' )
    , ( "Sie", 'ሼ' )
    , ( "So", 'ሾ' )
    , ( "Su", 'ሹ' )
    , ( "Sua", 'ሿ' )
    , ( "T", 'ጥ' )
    , ( "Ta", 'ጣ' )
    , ( "Te", 'ጠ' )
    , ( "Ti", 'ጢ' )
    , ( "Tie", 'ጤ' )
    , ( "To", 'ጦ' )
    , ( "Tu", 'ጡ' )
    , ( "Tua", 'ጧ' )
    , ( "Z", 'ዥ' )
    , ( "Za", 'ዣ' )
    , ( "Ze", 'ዠ' )
    , ( "Zi", 'ዢ' )
    , ( "Zie", 'ዤ' )
    , ( "Zo", 'ዦ' )
    , ( "Zu", 'ዡ' )
    , ( "Zua", 'ዧ' )
    , ( "a", 'ኣ' )
    , ( "b", 'ብ' )
    , ( "ba", 'ባ' )
    , ( "be", 'በ' )
    , ( "bi", 'ቢ' )
    , ( "bie", 'ቤ' )
    , ( "bo", 'ቦ' )
    , ( "bu", 'ቡ' )
    , ( "bua", 'ቧ' )
    , ( "c", 'ች' )
    , ( "ca", 'ቻ' )
    , ( "ce", 'ቸ' )
    , ( "ci", 'ቺ' )
    , ( "cie", 'ቼ' )
    , ( "co", 'ቾ' )
    , ( "cu", 'ቹ' )
    , ( "cua", 'ቿ' )
    , ( "d", 'ድ' )
    , ( "da", 'ዳ' )
    , ( "dd", 'ዽ' )
    , ( "dda", 'ዻ' )
    , ( "dde", 'ዸ' )
    , ( "ddi", 'ዺ' )
    , ( "ddie", 'ዼ' )
    , ( "ddo", 'ዾ' )
    , ( "ddu", 'ዹ' )
    , ( "ddua", 'ዿ' )
    , ( "de", 'ደ' )
    , ( "di", 'ዲ' )
    , ( "die", 'ዴ' )
    , ( "do", 'ዶ' )
    , ( "du", 'ዱ' )
    , ( "dua", 'ዷ' )
    , ( "e", 'አ' )
    , ( "f", 'ፍ' )
    , ( "fa", 'ፋ' )
    , ( "fe", 'ፈ' )
    , ( "fi", 'ፊ' )
    , ( "fi2", 'ፚ' )
    , ( "fie", 'ፌ' )
    , ( "fo", 'ፎ' )
    , ( "fu", 'ፉ' )
    , ( "fua", 'ፏ' )
    , ( "g", 'ግ' )
    , ( "gW", 'ጕ' )
    , ( "ga", 'ጋ' )
    , ( "ge", 'ገ' )
    , ( "gi", 'ጊ' )
    , ( "gie", 'ጌ' )
    , ( "go", 'ጎ' )
    , ( "goa", 'ጏ' )
    , ( "gu", 'ጉ' )
    , ( "gua", 'ጓ' )
    , ( "gue", 'ጐ' )
    , ( "gui", 'ጒ' )
    , ( "guie", 'ጔ' )
    , ( "h", 'ህ' )
    , ( "hW", 'ኍ' )
    , ( "ha", 'ሃ' )
    , ( "he", 'ሀ' )
    , ( "hh", 'ኅ' )
    , ( "hha", 'ኃ' )
    , ( "hhe", 'ኀ' )
    , ( "hhi", 'ኂ' )
    , ( "hhie", 'ኄ' )
    , ( "hho", 'ኆ' )
    , ( "hhu", 'ኁ' )
    , ( "hi", 'ሂ' )
    , ( "hie", 'ሄ' )
    , ( "ho", 'ሆ' )
    , ( "hoa", 'ሇ' )
    , ( "hu", 'ሁ' )
    , ( "hua", 'ኋ' )
    , ( "hue", 'ኈ' )
    , ( "hui", 'ኊ' )
    , ( "huie", 'ኌ' )
    , ( "i", 'ኢ' )
    , ( "ie", 'ኤ' )
    , ( "j", 'ጅ' )
    , ( "ja", 'ጃ' )
    , ( "je", 'ጀ' )
    , ( "ji", 'ጂ' )
    , ( "jie", 'ጄ' )
    , ( "jo", 'ጆ' )
    , ( "ju", 'ጁ' )
    , ( "jua", 'ጇ' )
    , ( "k", 'ክ' )
    , ( "kW", 'ኵ' )
    , ( "ka", 'ካ' )
    , ( "ke", 'ከ' )
    , ( "ki", 'ኪ' )
    , ( "kie", 'ኬ' )
    , ( "ko", 'ኮ' )
    , ( "koa", 'ኯ' )
    , ( "ku", 'ኩ' )
    , ( "kua", 'ኳ' )
    , ( "kue", 'ኰ' )
    , ( "kui", 'ኲ' )
    , ( "kuie", 'ኴ' )
    , ( "l", 'ል' )
    , ( "la", 'ላ' )
    , ( "le", 'ለ' )
    , ( "li", 'ሊ' )
    , ( "lie", 'ሌ' )
    , ( "lo", 'ሎ' )
    , ( "lu", 'ሉ' )
    , ( "lua", 'ሏ' )
    , ( "m", 'ም' )
    , ( "ma", 'ማ' )
    , ( "me", 'መ' )
    , ( "mi", 'ሚ' )
    , ( "mi2", 'ፙ' )
    , ( "mie", 'ሜ' )
    , ( "mo", 'ሞ' )
    , ( "mu", 'ሙ' )
    , ( "mua", 'ሟ' )
    , ( "n", 'ን' )
    , ( "na", 'ና' )
    , ( "ne", 'ነ' )
    , ( "ni", 'ኒ' )
    , ( "nie", 'ኔ' )
    , ( "no", 'ኖ' )
    , ( "nu", 'ኑ' )
    , ( "nua", 'ኗ' )
    , ( "o", 'ኦ' )
    , ( "p", 'ፕ' )
    , ( "pa", 'ፓ' )
    , ( "pe", 'ፐ' )
    , ( "pi", 'ፒ' )
    , ( "pie", 'ፔ' )
    , ( "po", 'ፖ' )
    , ( "pu", 'ፑ' )
    , ( "pua", 'ፗ' )
    , ( "q", 'ቅ' )
    , ( "qW", 'ቍ' )
    , ( "qa", 'ቃ' )
    , ( "qe", 'ቀ' )
    , ( "qi", 'ቂ' )
    , ( "qie", 'ቄ' )
    , ( "qo", 'ቆ' )
    , ( "qoa", 'ቇ' )
    , ( "qu", 'ቁ' )
    , ( "qua", 'ቋ' )
    , ( "que", 'ቈ' )
    , ( "qui", 'ቊ' )
    , ( "quie", 'ቌ' )
    , ( "r", 'ር' )
    , ( "ra", 'ራ' )
    , ( "re", 'ረ' )
    , ( "ri", 'ሪ' )
    , ( "ri2", 'ፘ' )
    , ( "rie", 'ሬ' )
    , ( "ro", 'ሮ' )
    , ( "ru", 'ሩ' )
    , ( "rua", 'ሯ' )
    , ( "s", 'ስ' )
    , ( "s2ua", 'ሧ' )
    , ( "sa", 'ሳ' )
    , ( "se", 'ሰ' )
    , ( "si", 'ሲ' )
    , ( "sie", 'ሴ' )
    , ( "so", 'ሶ' )
    , ( "ss", 'ሥ' )
    , ( "ssa", 'ሣ' )
    , ( "sse", 'ሠ' )
    , ( "ssi", 'ሢ' )
    , ( "ssie", 'ሤ' )
    , ( "sso", 'ሦ' )
    , ( "ssu", 'ሡ' )
    , ( "su", 'ሱ' )
    , ( "sua", 'ሷ' )
    , ( "t", 'ት' )
    , ( "ta", 'ታ' )
    , ( "te", 'ተ' )
    , ( "ti", 'ቲ' )
    , ( "tie", 'ቴ' )
    , ( "to", 'ቶ' )
    , ( "tu", 'ቱ' )
    , ( "tua", 'ቷ' )
    , ( "u", 'ኡ' )
    , ( "ua", 'ኧ' )
    , ( "v", 'ቭ' )
    , ( "va", 'ቫ' )
    , ( "ve", 'ቨ' )
    , ( "vi", 'ቪ' )
    , ( "vie", 'ቬ' )
    , ( "vo", 'ቮ' )
    , ( "vu", 'ቩ' )
    , ( "vua", 'ቯ' )
    , ( "w", 'ው' )
    , ( "wa", 'ዋ' )
    , ( "we", 'ወ' )
    , ( "wi", 'ዊ' )
    , ( "wie", 'ዌ' )
    , ( "wo", 'ዎ' )
    , ( "woa", 'ዏ' )
    , ( "wu", 'ዉ' )
    , ( "x", 'ጽ' )
    , ( "x2ua", 'ፇ' )
    , ( "xa", 'ጻ' )
    , ( "xe", 'ጸ' )
    , ( "xi", 'ጺ' )
    , ( "xie", 'ጼ' )
    , ( "xo", 'ጾ' )
    , ( "xu", 'ጹ' )
    , ( "xua", 'ጿ' )
    , ( "xx", 'ፅ' )
    , ( "xxa", 'ፃ' )
    , ( "xxe", 'ፀ' )
    , ( "xxi", 'ፂ' )
    , ( "xxie", 'ፄ' )
    , ( "xxo", 'ፆ' )
    , ( "xxu", 'ፁ' )
    , ( "y", 'ይ' )
    , ( "ya", 'ያ' )
    , ( "ye", 'የ' )
    , ( "yi", 'ዪ' )
    , ( "yie", 'ዬ' )
    , ( "yo", 'ዮ' )
    , ( "yoa", 'ዯ' )
    , ( "yu", 'ዩ' )
    , ( "z", 'ዝ' )
    , ( "za", 'ዛ' )
    , ( "ze", 'ዘ' )
    , ( "zi", 'ዚ' )
    , ( "zie", 'ዜ' )
    , ( "zo", 'ዞ' )
    , ( "zu", 'ዙ' )
    , ( "zua", 'ዟ' )
    ]
