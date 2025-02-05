module Dictation exposing
    ( Nonempty(..)
    , consonantFour
    , consonantOne
    , consonantThree
    , consonantTwo
    , genAll
    , genFour
    , genFromList
    , genOne
    , genThree
    , genTwo
    , toList, all
    )

import Random exposing (Generator)


type Nonempty a
    = Nonempty a (List a)


head : Nonempty a -> a
head (Nonempty x _) =
    x


tail : Nonempty a -> List a
tail (Nonempty _ xs) =
    xs


toList : Nonempty a -> List a
toList (Nonempty x xs) =
    x :: xs


concat : Nonempty (Nonempty a) -> Nonempty a
concat (Nonempty xs xss) =
    let
        hd =
            head xs

        tl =
            tail xs ++ List.concat (List.map toList xss)
    in
    Nonempty hd tl


consonantOne : Nonempty Char
consonantOne =
    Nonempty 'ሀ' [ 'ለ', 'በ', 'መ', 'ነ', 'ረ', 'ሰ', 'ከ', 'ቀ' ]


consonantTwo : Nonempty Char
consonantTwo =
    Nonempty 'ወ' [ 'ተ', 'ቸ', 'ዘ', 'ደ', 'ጀ', 'አ', 'ፈ', 'ፐ' ]


consonantThree : Nonempty Char
consonantThree =
    Nonempty 'ሐ' [ 'ዐ', 'ኀ', 'ሸ', 'የ', 'ሠ', 'ኘ' ]


consonantFour : Nonempty Char
consonantFour =
    Nonempty 'ገ' [ 'ጠ', 'ጨ', 'ጰ', 'ጸ', 'ፀ', 'ዠ', 'ኸ' ]


basePatterns : Nonempty String
basePatterns =
    Nonempty "CVC" [ "CVCV", "CVCVC", "CVCC" ]

all : Nonempty Char
all =
    Nonempty consonantOne [ consonantTwo, consonantThree, consonantFour ]
        |> concat

randConsonant : Nonempty Char -> Generator Char
randConsonant (Nonempty x xs) =
    Random.uniform x xs


randPattern : Generator String
randPattern =
    Random.uniform (head basePatterns) (tail basePatterns)


randVowel : Nonempty Char -> Generator Char
randVowel list =
    let
        helper ( offset, letter ) =
            let
                off =
                    if offset == 7 && (letter == 'ዐ' || letter == 'ቐ' || letter == 'ኸ') then
                        offset - 1

                    else
                        offset
            in
            Char.fromCode <| Char.toCode letter + off
    in
    Random.map helper <|
        Random.pair (Random.int 1 7) (randConsonant list)


combineGenerators : List (Generator a) -> Generator (List a)
combineGenerators generators =
    List.foldr
        (\gen acc -> Random.map2 (::) gen acc)
        (Random.constant []) generators


randWord : Nonempty Char -> Generator String
randWord list =
    let
        helper pat =
            let
                randomChar c =
                    if c == 'C' then
                        randConsonant list

                    else
                        randVowel list

                charGenerators =
                    pat
                        |> String.toList
                        |> List.map randomChar
            in
            combineGenerators charGenerators
    in
    Random.andThen helper randPattern
        |> Random.map (\l -> String.fromList l)


genFromList : Int -> Nonempty Char -> Generator String
genFromList len list =
    randWord list
        |> Random.list len
        |> Random.map
            (\words ->
                List.foldr (++) "" <| List.intersperse " " words
            )


genOne : Int -> Generator String
genOne len =
    genFromList len consonantOne


genTwo : Int -> Generator String
genTwo len =
    genFromList len consonantTwo


genThree : Int -> Generator String
genThree len =
    genFromList len consonantThree


genFour : Int -> Generator String
genFour len =
    genFromList len consonantFour


genAll : Int -> Generator String
genAll len =
    all
        |> genFromList len
