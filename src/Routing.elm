module Routing exposing (Route(..), routeFromUrl)

import Url exposing (Url)


type Route
    = Home
    | Stats
    | NotFound


routeFromUrl : Url -> Route
routeFromUrl url =
    case url.fragment of
        Just "" ->
            Home

        Just "stats" ->
            Stats

        Nothing ->
            Home

        _ ->
            NotFound
