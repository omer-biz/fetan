module Types.KeyAttempt exposing (KeyAttempt(..))

{-| Represents the result of a key attempt, which can be:

  - `Wrong`: The key was incorrect.
  - `Correct`: The key was correct.
  - `Partial`: The key was partially correct.

-}


type KeyAttempt
    = Wrong
    | Correct
    | Partial
