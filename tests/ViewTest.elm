module ViewTest exposing (suite)

import Expect
import Test exposing (Test, describe, test)
import Types.Core exposing (KeyEvent)
import Types.Msg exposing (Msg(..))
import View exposing (dispatchDown, dispatchUp)


keyEvent : String -> KeyEvent
keyEvent code =
    { code = code, timeStamp = 100 }


suite : Test
suite =
    describe "keyboard event dispatch"
        [ test "Caps Lock down is a modifier event" <|
            \_ ->
                dispatchDown (keyEvent "CapsLock")
                    |> Expect.equal (ModKeyDown "CapsLock")
        , test "Caps Lock up is a modifier event" <|
            \_ ->
                dispatchUp (keyEvent "CapsLock")
                    |> Expect.equal (ModKeyUp "CapsLock")
        , test "letter keys remain typing events" <|
            \_ ->
                dispatchUp (keyEvent "KeyA")
                    |> Expect.equal (KeyUp (keyEvent "KeyA"))
        ]
