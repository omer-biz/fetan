module Update exposing (..)

import Array
import Browser
import Browser.Dom
import Browser.Navigation as Nav
import Community
import Dict exposing (Dict)
import Dictation as DictGen
import DictationLogic exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Models.Layout as Layout exposing (Layout(..))
import Ports
import Process
import Random
import Routing exposing (Route(..))
import Stats exposing (LetterStat, SessionRecord)
import Storage
import Task
import Time
import Types.Core exposing (..)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Types.KeyModifier exposing (KeyModifier(..))
import Types.Msg exposing (..)
import Url exposing (Url)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        keyboard =
            model.keyboard

        curLayout =
            model.currentLayout

        info =
            model.info

        dictation =
            model.dictation

        metrics =
            info.metrics

        updateKey key state =
            updateFirstOccurrence
                (\k -> key == k.code)
                (\k -> { k | state = state })
                keyboard.keys
    in
    case msg of
        FocusKeyBr ->
            ( { model | keyboard = { keyboard | focusKeyBr = True } }, Cmd.none )

        BlurKeyBr ->
            ( { model | keyboard = { keyboard | focusKeyBr = False } }, Cmd.none )

        KeyDown keyEvent ->
            if not model.keyboard.focusKeyBr || model.info.onboardingStep == 0 then
                ( model, Cmd.none )
            else
            let
                infoAfterOnboarding =
                    if model.info.onboardingStep == 1 then
                        { info | onboardingStep = 3 }
                    else if model.info.onboardingStep == 3 then
                        { info | onboardingStep = 4 }
                    else if model.info.onboardingStep == 5 then
                        { info | onboardingStep = 6 }
                    else if model.info.onboardingStep == 7 then
                        { info | onboardingStep = 8 }
                    else
                        model.info

                isFirstKey = not model.started
                
                newLastSuccessTime =
                    if isFirstKey then
                        keyEvent.timeStamp

                    else
                        model.lastSuccessTime
                        
                newSessionStartTime =
                    if isFirstKey then
                        model.timeOrigin + keyEvent.timeStamp
                    else
                        model.sessionStartTime
            in
            ( { model 
                | keyboard = { keyboard | keys = updateKey keyEvent.code Pressed }
                , lastKeyEvent = 0
                , started = True
                , lastSuccessTime = newLastSuccessTime
                , sessionStartTime = newSessionStartTime 
                , info = infoAfterOnboarding
              }
            , if infoAfterOnboarding.onboardingStep /= model.info.onboardingStep then Ports.saveInfo (Storage.encodeInfo infoAfterOnboarding) else Cmd.none 
            )

        KeyUp keyEvent ->
            if not model.keyboard.focusKeyBr || model.info.onboardingStep == 0 then
                ( model, Cmd.none )
            else
            let
                ( dict, layout, attemptResult ) =
                    updateDictation keyEvent.code keyboard.modifier model.currentLayout dictation

                newErrors =
                    if attemptResult == WasWrong then
                        Maybe.map (\c -> String.fromChar c.letter) dictation.current
                            |> Maybe.map (\char -> model.currentErrors ++ [ char ])
                            |> Maybe.withDefault model.currentErrors

                    else
                        model.currentErrors

                updates =
                    let
                        targetLetter =
                            Maybe.map (\curr -> String.fromChar curr.letter) dictation.current |> Maybe.withDefault ""

                        oldStat =
                            Dict.get targetLetter (getCurrentLayoutData info).letterStats |> Maybe.withDefault { errorEma = 0, latencyEma = 0, count = 0 }

                        ( updatedStats, updatedTime ) =
                            case attemptResult of
                                NoOpResult ->
                                    ( (getCurrentLayoutData info).letterStats, model.lastSuccessTime )

                                WasPartial ->
                                    ( (getCurrentLayoutData info).letterStats, model.lastSuccessTime )

                                WasWrong ->
                                    let
                                        newStat =
                                            { oldStat | errorEma = 0.1 * 1.0 + 0.9 * oldStat.errorEma }
                                    in
                                    ( Dict.insert targetLetter newStat (getCurrentLayoutData info).letterStats, model.lastSuccessTime )

                                WasCorrect ->
                                    let
                                        latency =
                                            keyEvent.timeStamp - model.lastSuccessTime

                                        newStat =
                                            if oldStat.count == 0 then
                                                { oldStat | count = 1, latencyEma = latency, errorEma = 0 }

                                            else
                                                { oldStat
                                                    | count = oldStat.count + 1
                                                    , latencyEma = 0.1 * latency + 0.9 * oldStat.latencyEma
                                                    , errorEma = 0.9 * oldStat.errorEma
                                                }
                                    in
                                    ( Dict.insert targetLetter newStat (getCurrentLayoutData info).letterStats, keyEvent.timeStamp )

                        isFinished =
                            dict.current == Nothing

                        ( finalLessonIdx, finalCompleted ) =
                            if isFinished then
                                if model.dictationMode == PracticeMode then
                                    ( (getCurrentLayoutData info).lessonIdx, (getCurrentLayoutData info).dictationsCompleted + 1 )
                                else
                                let
                                    baseLetter =
                                        getBaseLetterForLesson (getCurrentLayoutData info).lessonIdx

                                    stat =
                                        getFamilyStats baseLetter updatedStats

                                    accuracyScore =
                                        clamp 0 1 (1.0 - (stat.errorEma * 10))

                                    speedScore =
                                        clamp 0 1 ((2500 - stat.latencyEma) / 1300)

                                    conf =
                                        0.5 + (accuracyScore * 0.4) + (speedScore * 0.1)
                                in
                                if stat.count >= 15 && conf > 0.85 && (getCurrentLayoutData info).lessonIdx < 34 then
                                    ( (getCurrentLayoutData info).lessonIdx + 1, 0 )

                                else
                                    ( (getCurrentLayoutData info).lessonIdx, (getCurrentLayoutData info).dictationsCompleted + 1 )

                            else
                                ( (getCurrentLayoutData info).lessonIdx, (getCurrentLayoutData info).dictationsCompleted )
                    in
                    { nextLessonIdx = finalLessonIdx
                    , nextCompleted = finalCompleted
                    , nextStats = updatedStats
                    , newLastSuccessTime = updatedTime
                    , didLevelUp = finalLessonIdx > (getCurrentLayoutData info).lessonIdx
                    , nextLetter = getBaseLetterForLesson finalLessonIdx
                    }
            in
            ( { model
                | keyboard = { keyboard | keys = updateKey keyEvent.code Released }
                , currentLayout = layout
                , dictation = dict
                , info =
                    updateCurrentLayoutData
                        (\layoutData ->
                            { layoutData
                                | lessonIdx = updates.nextLessonIdx
                                , dictationsCompleted = updates.nextCompleted
                                , letterStats = updates.nextStats
                            }
                        )
                        info
                , lastSuccessTime = updates.newLastSuccessTime
                , currentErrors = newErrors
                , justLeveledUp = if updates.didLevelUp then True else model.justLeveledUp
              }
            , if dict.current == Nothing then
                let
                    nextCmd =
                        if model.dictationMode == PracticeMode then
                            let
                                sortedStats =
                                    Dict.toList updates.nextStats
                                        |> List.filter (\(_, stat) -> stat.count > 0)
                                        |> List.sortBy (\(_, stat) -> -stat.latencyEma)
                                        |> List.take 5
                                        |> List.map Tuple.first
                            in
                            DictGen.genForWeaknesses sortedStats updates.nextLessonIdx
                                |> Random.generate NewDict
                        else
                            DictGen.genForLevel updates.nextLessonIdx
                                |> Random.generate NewDict
                in
                Cmd.batch
                    [ nextCmd
                    , if updates.didLevelUp then Ports.triggerLevelUp (String.fromInt updates.nextLessonIdx) else Cmd.none
                    , if updates.didLevelUp then Task.perform (\_ -> ClearLevelUp) (Process.sleep 2000) else Cmd.none
                    ]

              else
                Cmd.none
            )

        NewDict dict ->
            let
                currList =
                    case dictation.current of
                        Just c ->
                            [ c ]

                        Nothing ->
                            []

                allChars =
                    List.concat [ dictation.prev, currList, dictation.next ]

                lenChars =
                    List.length allChars

                correctChars =
                    allChars
                        |> List.filter (\l -> l.wasWrong == False)
                        |> List.length

                isCompleted =
                    dictation.current == Nothing
                    
                newMetrics =
                    if model.time /= 0 && isCompleted then
                        -- only update global metrics if the dictation was fully completed
                        info.metrics
                            |> updateSpeed model.time lenChars
                            |> updateAccuracy lenChars correctChars

                    else
                        metrics
            in
            ( { model
                | dictation = stringToDictation dict
                , time = 0
                , lastKeyEvent = 0
                , started = False
                , currentErrors = []
                , info =
                    { info
                        | metrics = newMetrics
                        , history =
                            if model.time /= 0 && isCompleted then
                                let
                                    newRecord = { timestamp = model.currentTime, duration = model.time, wpm = newMetrics.speed.new, accuracy = newMetrics.accuracy.new, lessonIdx = (getCurrentLayoutData info).lessonIdx, errors = model.currentErrors, layoutKind = info.layoutKind }
                                    newHistory = info.history ++ [ newRecord ]
                                in
                                List.drop (max 0 (List.length newHistory - 200)) newHistory

                            else
                                info.history
                        , aggregate =
                            if model.time /= 0 then
                                { totalDuration = info.aggregate.totalDuration + model.time
                                , totalSessions = info.aggregate.totalSessions + 1
                                , topWpm = max info.aggregate.topWpm newMetrics.speed.new
                                , topAccuracy = max info.aggregate.topAccuracy newMetrics.accuracy.new
                                , sumWpm = info.aggregate.sumWpm + newMetrics.speed.new
                                , sumAccuracy = info.aggregate.sumAccuracy + newMetrics.accuracy.new
                                }
                            else
                                info.aggregate
                        , onboardingStep =
                            if model.time /= 0 then
                                if info.onboardingStep == 4 then
                                    5
                                else if info.onboardingStep == 6 then
                                    7
                                else
                                    info.onboardingStep
                            else
                                info.onboardingStep
                    }
              }
            , if model.time == 0 then
                Cmd.none

              else
                let
                    slowestLetter =
                        Dict.toList (getCurrentLayoutData info).letterStats
                            |> List.filter (\( _, s ) -> s.count > 0)
                            |> List.sortBy (\( _, s ) -> -s.latencyEma)
                            |> List.head
                            |> Maybe.map Tuple.first
                            |> Maybe.withDefault "N/A"

                    payload =
                        Encode.object
                            [ ( "wpm", Encode.int newMetrics.speed.new )
                            , ( "accuracy", Encode.int newMetrics.accuracy.new )
                            , ( "duration", Encode.float model.time )
                            , ( "lessonIdx", Encode.int (getCurrentLayoutData info).lessonIdx )
                            , ( "slowestLetter", Encode.string slowestLetter )
                            , ( "layoutKind", Encode.string info.layoutKind )
                            ]
                in
                Cmd.batch
                    [ Ports.saveInfo <| Storage.encodeInfo { info | metrics = newMetrics }
                    , Ports.trackEvent payload
                    ]
            )

        Tick posix ->
            let
                nowMillis =
                    toFloat (Time.posixToMillis posix)

                hints =
                    case dictation.current of
                        Just curr ->
                            Layout.hint curr.letter curLayout |> hintToList

                        Nothing ->
                            []

                hintedToList =
                    keyboard.keys
                        |> List.map (hintMod hints)

                keys =
                    if (model.lastKeyEvent > 0 || model.info.onboardingStep == 3) && model.keyboard.focusKeyBr then
                        hintedToList

                    else
                        keyboard.keys |> List.map (\k -> if k.state == Hinted then { k | state = Released } else k)
            in
            ( { model
                | time =
                    if model.started then
                        model.time + 1

                    else
                        0
                , currentTime = nowMillis
                , lastKeyEvent = model.lastKeyEvent + 1
                , keyboard = { keyboard | keys = keys }
              }
            , Cmd.none
            )

        ModKeyDown key ->
            let
                newState =
                    Layout.keyModDown key keyboard.modifier

                keys =
                    -- every time the user presses the mod keys the key rendering function gets called 47
                    -- times it's either this or storing the key views for plain, Shift, CapsLock, and ShiftCapslock
                    -- the classic tradeoff "storage or cpu" or hear me out here, I'm stupid. We will see.
                    -- I just hope the layout authors will not write heavy "renderer"
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    if k.code == key then
                                        { k | state = Pressed }

                                    else
                                        k

                                else
                                    { k | view = Layout.render newState k.code curLayout }
                            )
            in
            ( { model
                | keyboard = { keyboard | modifier = newState, keys = keys }
              }
            , Cmd.none
            )

        ModKeyUp key ->
            let
                newState =
                    Layout.keyModUp key keyboard.modifier

                keys =
                    -- same here: read the prev comment
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    { k | state = Released }

                                else
                                    { k | view = Layout.render newState k.code curLayout }
                            )
            in
            ( { model
                | keyboard = { keyboard | modifier = newState, keys = keys }
              }
            , Cmd.none
            )

        SelectLayout kind ->
            let
                newLayout =
                    Layout.initLayout kind

                keys =
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    k

                                else
                                    { k | view = Layout.render keyboard.modifier k.code newLayout }
                            )

                newInfo =
                    { info | layoutKind = layoutKindToString kind }
                    
                newLessonIdx =
                    (getCurrentLayoutData newInfo).lessonIdx
            in
            ( { model
                | layoutKind = kind
                , currentLayout = newLayout
                , keyboard = { keyboard | keys = keys }
                , info = newInfo
              }
            , Cmd.batch
                [ Ports.saveInfo <| Storage.encodeInfo newInfo
                , DictGen.genForLevel newLessonIdx |> Random.generate NewDict
                ]
            )

        CompleteLayoutSelection kind ->
            let
                newLayout =
                    Layout.initLayout kind

                keys =
                    keyboard.keys
                        |> List.map
                            (\k ->
                                if List.member k.code modifierKeys then
                                    k
                                else
                                    { k | view = Layout.render keyboard.modifier k.code newLayout }
                            )

                newKeyboard =
                    { keyboard | keys = keys }

                newInfo =
                    { info | layoutKind = layoutKindToString kind, onboardingStep = 1 }

                newModel =
                    { model | info = newInfo, layoutKind = kind, currentLayout = newLayout, keyboard = newKeyboard }
            in
            ( newModel
            , Cmd.batch
                [ Ports.saveInfo (Storage.encodeInfo newInfo)
                , Task.attempt (\_ -> NoOp) (Browser.Dom.focus "dictation-area")
                ]
            )
            
        SkipOnboarding ->
            let
                newInfo = { info | onboardingStep = 8 }
            in
            ( { model | info = newInfo }, Ports.saveInfo (Storage.encodeInfo newInfo) )

        DismissOnboarding ->
            let
                newInfo = { info | onboardingStep = info.onboardingStep + 1 }
            in
            ( { model | info = newInfo }, Ports.saveInfo (Storage.encodeInfo newInfo) )

        ToggleTheme ->
            let
                newTheme =
                    if model.theme == Dark then
                        Light

                    else
                        Dark
            in
            ( { model | theme = newTheme }
            , Ports.saveTheme
                (if newTheme == Dark then
                    "dark"

                 else
                    "light"
                )
            )

        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                newRoute =
                    routeFromUrl url

                cmd =
                    if newRoute == CommunityRoute then
                        Ports.fetchCommunityStats ()
                    else if newRoute == PracticeRoute && model.dictationMode /= PracticeMode then
                        let
                            sortedStats =
                                Dict.toList (getCurrentLayoutData model.info).letterStats
                                    |> List.filter (\(_, stat) -> stat.count > 0)
                                    |> List.sortBy (\(_, stat) -> -stat.latencyEma)
                                    |> List.take 5
                                    |> List.map Tuple.first
                        in
                        DictGen.genForWeaknesses sortedStats (getCurrentLayoutData model.info).lessonIdx |> Random.generate NewDict
                    else if newRoute == TypingRoute && model.dictationMode /= LessonMode then
                        DictGen.genForLevel (getCurrentLayoutData model.info).lessonIdx |> Random.generate NewDict
                    else
                        Cmd.none
                
                newMode =
                    if newRoute == PracticeRoute then
                        PracticeMode
                    else if newRoute == TypingRoute then
                        LessonMode
                    else
                        model.dictationMode
            in
            ( { model | route = newRoute, dictationMode = newMode }, cmd )

        GoTo route ->
            let
                urlStr =
                    case route of
                        TypingRoute ->
                            "/"

                        StatsRoute ->
                            "/#stats"

                        CommunityRoute ->
                            "/#community"

                        PracticeRoute ->
                            "/#practice"
            in
            ( model, Nav.pushUrl model.navKey urlStr )

        OnHoverStats items ->
            ( { model | hoveringStats = items }, Cmd.none )

        OnHoverMastery items ->
            ( { model | hoveringMastery = items }, Cmd.none )

        CommunityMsg cMsg ->
            let
                ( newComm, cCmd ) =
                    Community.update cMsg model.communityData
            in
            ( { model | communityData = newComm }, Cmd.map CommunityMsg cCmd )

        GotCommunityStats val ->
            ( { model | communityData = Community.handleReceiveStats val model.communityData }, Cmd.none )

        GotTimeZone zone ->
            ( { model | zone = zone }, Cmd.none )

        ClearLevelUp ->
            ( { model | justLeveledUp = False }, Cmd.none )

        StartPracticeMode ->
            let
                sortedStats =
                    Dict.toList (getCurrentLayoutData model.info).letterStats
                        |> List.filter (\(_, stat) -> stat.count > 0)
                        |> List.sortBy (\(_, stat) -> -stat.latencyEma)
                        |> List.take 5
                        |> List.map Tuple.first
            in
            ( { model | dictationMode = PracticeMode }
            , Cmd.batch 
                [ DictGen.genForWeaknesses sortedStats (getCurrentLayoutData model.info).lessonIdx |> Random.generate NewDict
                , Nav.pushUrl model.navKey "/#practice"
                ]
            )

        StartLessonMode ->
            ( { model | dictationMode = LessonMode }
            , Cmd.batch 
                [ DictGen.genForLevel (getCurrentLayoutData model.info).lessonIdx |> Random.generate NewDict
                , Nav.pushUrl model.navKey "/"
                ]
            )

        _ ->
            ( model, Cmd.none )


hintMod : List String -> Key -> Key
hintMod hints key =
    if List.member key.code hints then
        { key | state = Hinted }

    else if key.state == Hinted then
        { key | state = Released }

    else
        key


bestShiftForKey : String -> String
bestShiftForKey key =
    let
        letter =
            String.dropLeft 3 key

        leftHandKeys =
            [ "Q", "W", "E", "R", "T", "A", "S", "D", "F", "G", "Z", "X", "C", "V", "B" ]
    in
    if List.member letter leftHandKeys then
        "ShiftRight"

    else
        "ShiftLeft"


hintToList : Maybe ( KeyModifier, String ) -> List String
hintToList hint =
    let
        modToCode mod code =
            case mod of
                NoModifier ->
                    []

                CapsLock ->
                    [ "CapsLock" ]

                Shift ->
                    [ bestShiftForKey code ]

                ShiftCapsLock ->
                    [ "ShiftRight", "CapsLock" ]
    in
    case hint of
        Just ( mod, code ) ->
            code :: modToCode mod code

        Nothing ->
            []





layoutKindToString : Layout.LayoutKind -> String
layoutKindToString kind =
    case kind of
        Layout.SilPowerG ->
            "SilPowerG"

        Layout.PowerGeez ->
            "PowerGeez"

        Layout.GeezIME ->
            "GeezIME"

modifierKeys : List String
modifierKeys =
    [ "ShiftLeft", "ShiftRight", "CapsLock", "ControlRight", "ControlLeft", "AltRight", "AltLeft", "Tab", "MetaLeft", "MetaRight", "Enter" ]
