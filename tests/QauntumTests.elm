module QauntumTests exposing (..)

import Expect exposing (Expectation)
import Field
import Fuzz exposing (Fuzzer, int, list, string)
import Monoid
import Quantum
import Result
import Test exposing (..)
import Vector


suite : Test.Test
suite =
    Test.describe "Quantum Tests"
        [ Test.fuzz2
            Fuzz.float
            Fuzz.float
            "tests Ket add is commutative"
          <|
            \one two ->
                let
                    v =
                        Quantum.ket Monoid.numberSum
                            (Vector.Vector
                                [ one
                                , two
                                ]
                            )

                    w =
                        Quantum.ket Monoid.numberSum
                            (Vector.Vector
                                [ one
                                , two
                                ]
                            )
                in
                Result.map2 (Quantum.add Field.float) v w
                    |> Expect.equal (Result.map2 (Quantum.add Field.float) w v)
        ]
