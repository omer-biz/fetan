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
        [ test "Empty fragment goes to Home" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "" }
                    |> Expect.equal Home
        , test "Nothing fragment goes to Home" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Nothing }
                    |> Expect.equal Home
        , test "Stats fragment goes to Stats" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "stats" }
                    |> Expect.equal Stats
        , test "Unknown fragment goes to NotFound" <|
            \_ ->
                routeFromUrl { urlBase | fragment = Just "foo" }
                    |> Expect.equal NotFound
        ]
