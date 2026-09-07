port module Ports exposing (..)

import Json.Encode as Encode

port saveInfo : Encode.Value -> Cmd msg
port saveTheme : String -> Cmd msg
port saveAnalyticsConsent : Bool -> Cmd msg
port trackEvent : Encode.Value -> Cmd msg
port triggerLevelUp : String -> Cmd msg
port fetchCommunityStats : () -> Cmd msg
port receiveCommunityStats : (Encode.Value -> msg) -> Sub msg
