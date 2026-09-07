module Routing exposing (Route(..), routeFromUrl)

import Url exposing (Url)


type Route
    = TypingRoute
    | PracticeRoute
    | StatsRoute
    | CommunityRoute


routeFromUrl : Url -> Route
routeFromUrl url =
    case url.fragment of
        Just "stats" ->
            StatsRoute

        Just "community" ->
            CommunityRoute

        Just "practice" ->
            PracticeRoute

        _ ->
            TypingRoute
