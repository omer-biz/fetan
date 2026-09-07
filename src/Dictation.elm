module Dictation exposing
    ( genForLevel
    , genForWeaknesses
    , lessonCount
    , learningSequence
    )

import Dict exposing (Dict)
import Random exposing (Generator)
import Words


learningSequence : List Char
learningSequence =
    [ 'መ', 'ተ', 'በ', 'ነ', 'ረ', 'ለ', 'የ', 'ወ', 'ሰ', 'አ', 'ከ', 'ደ', 'ገ', 'ሀ', 'ቀ', 'ቸ', 'ፈ', 'ጠ', 'ዘ', 'ጀ', 'ኘ', 'ሸ', 'ጨ', 'ሐ', 'ሠ', 'ዐ', 'ጸ', 'ፀ', 'ፐ', 'ጰ', 'ኀ', 'ዠ', 'ኸ', 'ቨ' ]


lessonCount : Int
lessonCount =
    List.length learningSequence


genForLevel : Int -> Generator String
genForLevel level =
    let
        effLevel =
            clamp 1 lessonCount level

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
        |> Random.list 12
        |> Random.map (String.join " ")

genForWeaknesses : List String -> Int -> Generator String
genForWeaknesses weakLetters currentLesson =
    let
        effLevel =
            clamp 1 lessonCount currentLesson

        -- Gather all words up to the current lesson
        availableWords =
            Dict.toList Words.byLesson
                |> List.filter (\(l, _) -> l <= effLevel)
                |> List.concatMap Tuple.second

        -- Filter words that contain at least one weak letter
        usefulWords =
            availableWords
                |> List.filter (\w -> List.any (\wl -> String.contains wl w) weakLetters)

        safeWords =
            if List.isEmpty usefulWords then
                if List.isEmpty availableWords then
                    [ "ሀለበመ" ]
                else
                    availableWords
            else
                usefulWords

        randWord =
            case safeWords of
                [] ->
                    Random.constant "ሀለበመ"

                x :: xs ->
                    Random.uniform x xs
    in
    randWord
        |> Random.list 12
        |> Random.map (String.join " ")
