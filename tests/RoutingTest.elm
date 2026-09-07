module RoutingTest exposing (..)

import Expect
import Routing exposing (Route(..), routeFromUrl)
import Test exposing (..)
import Url exposing (Protocol(..), Url)

urlBase : Url
urlBase =
    { protocol = Http
    , host = "localhost"
    , port_ = Just 8000
    , path = "/"
    , query = Nothing
    , fragment = Nothing
    }

suite : Test
suite =
    describe "Routing"
        [ test "Empty fragment goes to typing" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "" }
                    |> Expect.equal TypingRoute
        , test "Nothing fragment goes to typing" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Nothing }
                    |> Expect.equal TypingRoute
        , test "Stats fragment goes to Stats" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "stats" }
                    |> Expect.equal StatsRoute
        , test "Practice fragment goes to practice" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "practice" }
                    |> Expect.equal PracticeRoute
        , test "Community fragment goes to community" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "community" }
                    |> Expect.equal CommunityRoute
        , test "Unknown fragment goes to typing" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "foo" }
                    |> Expect.equal TypingRoute
        ]
