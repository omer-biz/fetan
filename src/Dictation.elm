module Dictation exposing
    ( genForLevel
    , learningSequence
    )

import Dict exposing (Dict)
import Random exposing (Generator)
import Words


learningSequence : List Char
learningSequence =
    [ 'መ', 'ተ', 'በ', 'ነ', 'ረ', 'ለ', 'የ', 'ወ', 'ሰ', 'አ', 'ከ', 'ደ', 'ገ', 'ሀ', 'ቀ', 'ቸ', 'ፈ', 'ጠ', 'ዘ', 'ጀ', 'ኘ', 'ሸ', 'ጨ', 'ሐ', 'ሠ', 'ዐ', 'ጸ', 'ፀ', 'ፐ', 'ጰ', 'ኀ', 'ዠ', 'ኸ', 'ቨ' ]


genForLevel : Int -> Generator String
genForLevel level =
    let
        effLevel =
            clamp 1 33 level

        wordsList =
            Dict.get effLevel Words.byLesson
                |> Maybe.withDefault [ "ሀለበመ" ]

        safeWords =
            if List.isEmpty wordsList then
                [ "ሀለበመ" ]

            else
                wordsList

        randWord =
            case safeWords of
                [] ->
                    Random.constant "ሀለበመ"

                x :: xs ->
                    Random.uniform x xs
    in
    randWord
        |> Random.list 8
        |> Random.map (String.join " ")
