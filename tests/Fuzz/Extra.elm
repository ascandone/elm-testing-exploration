module Fuzz.Extra exposing
    ( negativeInt
    , nonNegativeInt
    , positiveInt
    )

import Fuzz


nonNegativeInt : Fuzz.Fuzzer Int
nonNegativeInt =
    Fuzz.int
        |> Fuzz.map
            (\n ->
                if n >= 0 then
                    n

                else
                    -n
            )


positiveInt : Fuzz.Fuzzer Int
positiveInt =
    Fuzz.map ((+) 1) nonNegativeInt


negativeInt : Fuzz.Fuzzer Int
negativeInt =
    Fuzz.map negate positiveInt
