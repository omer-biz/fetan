module DictationLogic exposing (..)

import Dict exposing (Dict)
import Array
import Dictation as DictGen
import Models.Layout as Layout exposing (Layout)
import Stats exposing (LetterStat)
import Types.Core exposing (..)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))

wordCount : number
wordCount =
    10


stringToDictation : String -> Dictation
stringToDictation str =
    case String.uncons str of
        Just ( curr, next ) ->
            Dictation [] (Just (Letter curr Fresh False)) (lettersFromString next)

        Nothing ->
            Dictation [] Nothing []


lettersFromString : String -> List Letter
lettersFromString str =
    str
        |> String.toList
        |> List.map (\l -> Letter l Fresh False)


specialKeys : Dict String String
specialKeys =
    Dict.fromList
        [ ( "Tab", "flex-grow" )
        , ( "CapsLock", "w-20" )
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


updateFirstOccurrence : (a -> Bool) -> (a -> a) -> List a -> List a
updateFirstOccurrence predicate modVal list =
    let
        helper seen remaining =
            case remaining of
                [] ->
                    List.reverse seen

                x :: xs ->
                    if predicate x then
                        List.reverse seen ++ (modVal x :: xs)

                    else
                        helper (x :: seen) xs
    in
    helper [] list


getFamilyStats : String -> Dict String LetterStat -> LetterStat
getFamilyStats baseLetter stats =
    let
        baseCode =
            case String.uncons baseLetter of
                Just ( c, _ ) ->
                    Char.toCode c

                Nothing ->
                    0

        familyMembers =
            Dict.filter
                (\k _ ->
                    case String.uncons k of
                        Just ( c, _ ) ->
                            let
                                code =
                                    Char.toCode c
                            in
                            code >= baseCode && code < baseCode + 8

                        Nothing ->
                            False
                )
                stats

        totalCount =
            Dict.foldl (\_ v acc -> acc + v.count) 0 familyMembers

        avgLatency =
            if totalCount == 0 then
                0

            else
                (Dict.foldl (\_ v acc -> acc + (v.latencyEma * toFloat v.count)) 0 familyMembers) / toFloat totalCount

        avgError =
            if totalCount == 0 then
                0

            else
                (Dict.foldl (\_ v acc -> acc + (v.errorEma * toFloat v.count)) 0 familyMembers) / toFloat totalCount
    in
    { count = totalCount, latencyEma = avgLatency, errorEma = avgError }

updateDictation :
    String
    -> KeyModifier
    -> Layout
    -> Dictation
    -> ( Dictation, Layout, AttemptResult )
updateDictation codePoint keybrState layout dictation =
    case dictation.current of
        Just current ->
            let
                advanceDictation =
                    case dictation.next of
                        newCurr :: next ->
                            { dictation
                                | next = next
                                , current = Just newCurr
                                , prev = current :: dictation.prev
                            }

                        [] ->
                            { dictation | current = Nothing, prev = current :: dictation.prev }

                wrongAttempt =
                    { dictation | current = Just { current | state = Incorrect, wasWrong = True } }

                rollingCurrent =
                    { dictation | current = Just { current | state = Rolling } }
            in
            case Layout.update keybrState codePoint current.letter layout of
                ( newLayout, Partial ) ->
                    ( rollingCurrent, newLayout, WasPartial )

                ( newLayout, Correct ) ->
                    ( advanceDictation, newLayout, WasCorrect )

                ( newLayout, Wrong ) ->
                    ( wrongAttempt, newLayout, WasWrong )

        Nothing ->
            ( dictation, layout, NoOpResult )


updateSpeed : Float -> Int -> Metrics -> Metrics
updateSpeed time lenChars metrics =
    let
        speed =
            { old = metrics.speed.new, new = round <| (toFloat lenChars / 5) / (time / 60) }
    in
    { metrics | speed = speed }


updateAccuracy : Int -> Int -> Metrics -> Metrics
updateAccuracy totalChars correctChars metrics =
    let
        accuracy =
            { old = metrics.accuracy.new, new = round <| (toFloat correctChars * 100) / toFloat totalChars }
    in
    { metrics | accuracy = accuracy }


getBaseLetterForLesson : Int -> String
getBaseLetterForLesson idx =
    let
        learningSequence =
            Array.fromList (List.map String.fromChar DictGen.learningSequence)
    in
    Array.get (idx - 1) learningSequence |> Maybe.withDefault "ሀ"

getCurrentLayoutData : Info -> LayoutData
getCurrentLayoutData info =
    Dict.get info.layoutKind info.layouts
        |> Maybe.withDefault (LayoutData 4 0 Dict.empty)

updateCurrentLayoutData : (LayoutData -> LayoutData) -> Info -> Info
updateCurrentLayoutData updater info =
    let
        current =
            getCurrentLayoutData info
        
        updated =
            updater current
            
        newLayouts =
            Dict.insert info.layoutKind updated info.layouts
    in
    { info | layouts = newLayouts }
