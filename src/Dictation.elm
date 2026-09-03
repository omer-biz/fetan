module Dictation exposing
    ( genForLevel
    , learningSequence
    )

import Random exposing (Generator)
import Words
import Dict exposing (Dict)

learningSequence : List Char
learningSequence =
    [ 'ሀ', 'ለ', 'በ', 'መ', 'ነ', 'ረ', 'ሰ', 'ከ', 'ቀ', 'ወ', 'ተ', 'ቸ', 'ዘ', 'ደ', 'ጀ', 'አ', 'ፈ', 'ፐ', 'ሐ', 'ዐ', 'ኀ', 'ሸ', 'የ', 'ሠ', 'ኘ', 'ገ', 'ጠ', 'ጨ', 'ጰ', 'ጸ', 'ፀ', 'ዠ', 'ኸ' ]

genForLevel : Int -> Generator String
genForLevel level =
    let
        wordsList = 
            Dict.get level Words.byLesson 
                |> Maybe.withDefault [ "ሀለበመ" ]
        
        safeWords = 
            if List.isEmpty wordsList then
                [ "ሀለበመ" ]
            else
                wordsList
                
        randWord = 
            case safeWords of
                [] -> Random.constant "ሀለበመ"
                (x::xs) -> Random.uniform x xs
    in
    randWord
        |> Random.list 8
        |> Random.map (String.join " ")
