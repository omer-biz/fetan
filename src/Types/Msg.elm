module Types.Msg exposing (..)

import Browser
import Chart.Item as CI
import Community
import Json.Encode as Encode
import Models.Layout as Layout
import Stats exposing (LetterStat, SessionRecord)
import Time
import Types.Core exposing (..)
import Url exposing (Url)

type Msg
    = NoOp
    | KeyDown KeyEvent
    | KeyUp KeyEvent
    | ModKeyDown String
    | ModKeyUp String
    | FocusKeyBr
    | BlurKeyBr
    | NewDict String
    | Tick Time.Posix
    | SelectLayout Layout.LayoutKind
    | ToggleTheme
    | UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GoTo Route
    | CommunityMsg Community.Msg
    | GotCommunityStats Encode.Value
    | OnHoverStats (List (CI.One { index : Float, record : SessionRecord } CI.Dot))
    | OnHoverMastery (List (CI.One { index : Float, letter : String, stat : LetterStat } CI.Bar))
    | ClearLevelUp
    | GotTimeZone Time.Zone
