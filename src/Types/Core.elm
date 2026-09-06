module Types.Core exposing (..)

import Browser.Navigation as Nav
import Chart.Item as CI
import Community
import Dict exposing (Dict)
import Models.Layout as Layout exposing (Layout(..))
import Stats exposing (LetterStat, SessionRecord)
import Types.KeyAttempt exposing (KeyAttempt(..))
import Time
import Types.KeyModifier exposing (KeyModifier(..))
import Url exposing (Url)

type Theme
    = Light
    | Dark


type alias Model =
    { keyboard : Keyboard
    , dictation : Dictation
    , info : Info
    , time : Float
    , timeOrigin : Float
    , zone : Time.Zone
    , sessionStartTime : Float
    , currentTime : Float
    , lastSuccessTime : Float
    , lastKeyEvent : Float
    , currentLayout : Layout
    , layoutKind : Layout.LayoutKind
    , started : Bool
    , theme : Theme
    , currentErrors : List String
    , navKey : Nav.Key
    , route : Route
    , hoveringStats : List (CI.One { index : Float, record : SessionRecord } CI.Dot)
    , hoveringMastery : List (CI.One { index : Float, letter : String, stat : LetterStat } CI.Bar)
    , communityData : Community.Model
    , justLeveledUp : Bool
    }


type Route
    = TypingRoute
    | StatsRoute
    | CommunityRoute


routeFromUrl : Url -> Route
routeFromUrl url =
    if url.fragment == Just "stats" then
        StatsRoute

    else if url.fragment == Just "community" then
        CommunityRoute

    else
        TypingRoute



type alias LayoutData =
    { lessonIdx : Int
    , dictationsCompleted : Int
    , letterStats : Dict.Dict String LetterStat
    }

type alias Info =
    { metrics : Metrics
    , layoutKind : String
    , history : List SessionRecord
    , layouts : Dict.Dict String LayoutData
    }



type alias Metrics =
    { speed : { old : Int, new : Int }
    , accuracy : { old : Int, new : Int }
    }


type alias Dictation =
    { prev : List Letter
    , current : Maybe Letter
    , next : List Letter
    }


type LetterState
    = Fresh
    | Incorrect
    | Rolling


type alias Letter =
    { letter : Char
    , state : LetterState
    , wasWrong : Bool -- redundent
    }


type alias Keyboard =
    { focusKeyBr : Bool
    , modifier : KeyModifier
    , keys : List Key
    }


type KeyState
    = Pressed
    | Released
    | Hinted


type alias Key =
    { view : String
    , code : String
    , state : KeyState
    }


type AttemptResult
    = WasCorrect
    | WasWrong
    | WasPartial
    | NoOpResult


type alias KeyEvent =
    { code : String
    , timeStamp : Float
    }

