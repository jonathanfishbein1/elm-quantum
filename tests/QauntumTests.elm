module QauntumTests exposing (..)

import Expect
import Field
import Fuzz
import Quantum
import Test
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
                        Quantum.Ket
                            (Vector.Vector
                                [ one
                                , two
                                ]
                            )

                    w =
                        Quantum.Ket
                            (Vector.Vector
                                [ one
                                , two
                                ]
                            )
                in
                Quantum.add Field.float v w
                    |> Expect.equal (Quantum.add Field.float w v)
        ]
