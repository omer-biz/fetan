module Community exposing (Model, Msg(..), init, update, view, subscriptions, handleReceiveStats)

import Html exposing (Html)
import Html.Attributes exposing (class)
import Json.Decode as Decode exposing (Decoder)
import Chart as C
import Chart.Attributes as CA
import Chart.Events as CE
import Chart.Item as CI
import Dict exposing (Dict)

type alias LessonCount = { index : Float, lessonIdx : Int, count : Int }
type alias LetterCount = { index : Float, letter : String, count : Int }

type Status
    = Loading
    | Loaded (List CommunitySession)
    | Error String

type alias Model =
    { status : Status
    , hoveringLesson : List (CI.One LessonCount CI.Bar)
    , hoveringLetter : List (CI.One LetterCount CI.Bar)
    }

type alias CommunitySession =
    { wpm : Int
    , accuracy : Int
    , duration : Float
    , lessonIdx : Int
    , slowestLetter : String
    , timestamp : String
    }

type Msg
    = NoOp
    | OnHoverLesson (List (CI.One LessonCount CI.Bar))
    | OnHoverLetter (List (CI.One LetterCount CI.Bar))

init : Model
init =
    { status = Loading
    , hoveringLesson = []
    , hoveringLetter = []
    }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnHoverLesson items ->
            ( { model | hoveringLesson = items }, Cmd.none )

        OnHoverLetter items ->
            ( { model | hoveringLetter = items }, Cmd.none )

handleReceiveStats : Decode.Value -> Model -> Model
handleReceiveStats value model =
    case Decode.decodeValue (Decode.list sessionDecoder) value of
        Ok sessions ->
            { model | status = Loaded sessions }
        Err err ->
            { model | status = Error (Decode.errorToString err) }

sessionDecoder : Decoder CommunitySession
sessionDecoder =
    Decode.map6 CommunitySession
        (Decode.field "wpm" Decode.int)
        (Decode.field "accuracy" Decode.int)
        (Decode.field "duration" Decode.float)
        (Decode.field "lessonIdx" Decode.int)
        (Decode.field "slowestLetter" Decode.string)
        (Decode.field "timestamp" Decode.string)

-- View
view : Model -> Html Msg
view model =
    Html.div [ class "w-full max-w-[1000px] flex flex-col flex-1 mt-8 gap-8 pb-16" ]
        [ Html.div [ class "flex items-center justify-between" ]
            [ Html.h1 [ class "text-3xl font-bold text-stone-800 dark:text-stone-200" ] [ Html.text "Community Dashboard" ]
            ]
        , case model.status of
            Loading ->
                Html.div [ class "flex items-center justify-center py-20 text-stone-500" ] [ Html.text "Loading community pulse..." ]
            
            Error e ->
                Html.div [ class "p-4 bg-red-100 text-red-700 rounded-lg" ] [ Html.text ("Failed to load data: " ++ e) ]

            Loaded sessions ->
                if List.isEmpty sessions then
                    Html.div [ class "flex items-center justify-center py-20 text-stone-500" ] [ Html.text "No community data available yet." ]
                else
                    viewDashboard sessions model
        ]

viewDashboard : List CommunitySession -> Model -> Html Msg
viewDashboard sessions model =
    let
        total = List.length sessions
        avgWpm = (List.map (\s -> toFloat s.wpm) sessions |> List.sum) / toFloat total
        avgAcc = (List.map (\s -> toFloat s.accuracy) sessions |> List.sum) / toFloat total

        -- Group by lessonIdx
        lessonCounts =
            List.foldl (\s acc -> 
                let 
                    count = Dict.get s.lessonIdx acc |> Maybe.withDefault 0 
                in 
                Dict.insert s.lessonIdx (count + 1) acc
            ) Dict.empty sessions
            |> Dict.toList
            |> List.sortBy Tuple.first
            |> List.indexedMap (\i (idx, c) -> { index = toFloat i, lessonIdx = idx, count = c })

        -- Group by slowestLetter
        letterCounts =
            List.foldl (\s acc ->
                if s.slowestLetter == "N/A" || s.slowestLetter == "" then acc else
                let
                    count = Dict.get s.slowestLetter acc |> Maybe.withDefault 0
                in
                Dict.insert s.slowestLetter (count + 1) acc
            ) Dict.empty sessions
            |> Dict.toList
            |> List.sortBy (\(_, count) -> -count)
            |> List.take 10
            |> List.indexedMap (\i (l, c) -> { index = toFloat i, letter = l, count = c })
    in
    Html.div [ class "flex flex-col gap-8 w-full" ]
        [ Html.div [ class "grid grid-cols-2 md:grid-cols-4 gap-4" ]
            [ statCard "Recent Sessions" (String.fromInt total)
            , statCard "Avg Speed" (String.fromInt (round avgWpm) ++ " wpm")
            , statCard "Avg Accuracy" (String.fromInt (round avgAcc) ++ "%")
            , statCard "Hardest Letter" (List.head letterCounts |> Maybe.map .letter |> Maybe.withDefault "-")
            ]
        , Html.div [ class "grid grid-cols-1 md:grid-cols-2 gap-8" ]
            [ viewBarChart "Lesson Drop-off" "Number of sessions played at each level" lessonCounts model.hoveringLesson
            , viewLetterChart "Biggest Pain Points" "Most frequent slowest letters" letterCounts model.hoveringLetter
            ]
        ]

statCard : String -> String -> Html Msg
statCard label value =
    Html.div [ class "bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-6 flex flex-col justify-center gap-1" ]
        [ Html.div [ class "text-xs font-bold tracking-widest text-slate-400 dark:text-slate-500 uppercase" ] [ Html.text label ]
        , Html.div [ class "text-3xl font-black text-slate-700 dark:text-slate-200" ] [ Html.text value ]
        ]

viewBarChart : String -> String -> List LessonCount -> List (CI.One LessonCount CI.Bar) -> Html Msg
viewBarChart title desc data hovering =
    Html.div [ class "bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-8 h-[350px] flex flex-col" ]
        [ Html.div [ class "mb-4" ]
            [ Html.h2 [ class "text-lg font-semibold text-stone-800 dark:text-stone-200" ] [ Html.text title ]
            , Html.p [ class "text-sm text-stone-500 dark:text-stone-400 mt-1" ] [ Html.text desc ]
            ]
        , Html.div [ class "flex-1 w-full" ]
            [ C.chart
                [ CA.height 250
                , CA.width 400
                , CA.margin { top = 20, bottom = 45, left = 55, right = 40 }
                , CE.onMouseMove OnHoverLesson (CE.getNearest CI.bars)
                , CE.onMouseLeave (OnHoverLesson [])
                ]
                [ C.xLabels 
                    [ CA.color "var(--chart-text)"
                    , CA.format (\x -> 
                        List.head (List.drop (round x - 1) data) 
                            |> Maybe.map (\d -> String.fromInt d.lessonIdx) 
                            |> Maybe.withDefault ""
                      )
                    , CA.amount (List.length data)
                    ]
                , C.yLabels [ CA.withGrid, CA.color "var(--chart-text)" ]
                , C.grid [ CA.color "var(--chart-grid)", CA.width 1 ]
                , C.bars
                    [ CA.margin 0.2 ]
                    [ C.bar (\d -> toFloat d.count) [ CA.color "var(--chart-primary)" ] ]
                    data
                , C.each hovering <| \p item ->
                    let rec = (CI.getData item) in
                    [ C.tooltip item [] [] 
                        [ Html.div [ class "flex flex-col gap-1 text-sm text-stone-700 dark:text-stone-300 bg-white dark:bg-stone-900 p-3 rounded-lg shadow-xl border border-stone-200 dark:border-stone-800 z-50" ] 
                            [ Html.span [ class "font-bold" ] [ Html.text ("Level " ++ String.fromInt rec.lessonIdx) ]
                            , Html.span [] [ Html.text (String.fromInt rec.count ++ " sessions") ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

viewLetterChart : String -> String -> List LetterCount -> List (CI.One LetterCount CI.Bar) -> Html Msg
viewLetterChart title desc data hovering =
    Html.div [ class "bg-white dark:bg-stone-800/80 rounded-xl shadow-[0_2px_12px_rgb(0,0,0,0.04)] dark:shadow-none border border-stone-200 dark:border-stone-700 p-8 h-[350px] flex flex-col" ]
        [ Html.div [ class "mb-4" ]
            [ Html.h2 [ class "text-lg font-semibold text-stone-800 dark:text-stone-200" ] [ Html.text title ]
            , Html.p [ class "text-sm text-stone-500 dark:text-stone-400 mt-1" ] [ Html.text desc ]
            ]
        , Html.div [ class "flex-1 w-full" ]
            [ C.chart
                [ CA.height 250
                , CA.width 400
                , CA.margin { top = 20, bottom = 45, left = 55, right = 40 }
                , CE.onMouseMove OnHoverLetter (CE.getNearest CI.bars)
                , CE.onMouseLeave (OnHoverLetter [])
                ]
                [ C.xLabels 
                    [ CA.color "var(--chart-text)"
                    , CA.format (\x -> 
                        List.head (List.drop (round x - 1) data) 
                            |> Maybe.map .letter 
                            |> Maybe.withDefault ""
                      )
                    , CA.amount (List.length data)
                    ]
                , C.yLabels [ CA.withGrid, CA.color "var(--chart-text)" ]
                , C.grid [ CA.color "var(--chart-grid)", CA.width 1 ]
                , C.bars
                    [ CA.margin 0.2 ]
                    [ C.bar (\d -> toFloat d.count) [ CA.color "var(--chart-secondary)" ] ]
                    data
                , C.each hovering <| \p item ->
                    let rec = (CI.getData item) in
                    [ C.tooltip item [] [] 
                        [ Html.div [ class "flex flex-col gap-1 text-sm text-stone-700 dark:text-stone-300 bg-white dark:bg-stone-900 p-3 rounded-lg shadow-xl border border-stone-200 dark:border-stone-800 z-50" ] 
                            [ Html.span [ class "font-bold text-lg" ] [ Html.text rec.letter ]
                            , Html.span [] [ Html.text (String.fromInt rec.count ++ " sessions") ]
                            ]
                        ]
                    ]
                ]
            ]
        ]

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
