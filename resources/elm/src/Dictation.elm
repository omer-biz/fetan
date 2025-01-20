module Dictation exposing (genFour, genOne, genThree, genTwo, genAll)

import Random exposing (Generator)


consonantOne : List Char
consonantOne =
    [ 'ሀ', 'ለ', 'ሐ', 'መ', 'ሠ', 'ረ', 'ሰ', 'ሸ', 'ቀ' ]


consonantTwo : List Char
consonantTwo =
    [ 'በ', 'ተ', 'ቸ', 'ኀ', 'ነ', 'ኘ', 'አ', 'ከ', 'ኸ' ]


consonantThree : List Char
consonantThree =
    [ 'ወ', 'ዐ', 'ዘ', 'ዠ', 'የ', 'ደ', 'ጀ' ]


consonantFour : List Char
consonantFour =
    [ 'ገ', 'ጠ', 'ጨ', 'ጰ', 'ጸ', 'ፀ', 'ፈ', 'ፐ' ]


basePatterns : List String
basePatterns =
    [ "CVC", "CVCV", "CVCVC", "CVCC" ]


randConsonant : List Char -> Generator Char
randConsonant list =
    Random.uniform 'አ' list


randPattern : Generator String
randPattern =
    Random.uniform "CV" basePatterns


randVowel : List Char -> Generator Char
randVowel list =
    let
        helper ( offset, letter ) =
            let
                off =
                    if offset == 7 &&  (letter == 'ዐ' || letter == 'ቐ' || letter == 'ኸ') then
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
    case generators of
        [] ->
            Random.constant []

        generator :: rest ->
            Random.map2 (::) generator (combineGenerators rest)


randWord : List Char -> Generator String
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


genFromList : Int -> List Char -> Generator String
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
    [ consonantOne, consonantTwo, consonantThree, consonantFour ]
        |> List.concat
        |> genFromList len
