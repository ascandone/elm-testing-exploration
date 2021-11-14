module Test.Effect exposing (suite)

import Effect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "Effects"
        [ Effect.suite
        ]
