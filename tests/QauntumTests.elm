module QauntumTests exposing (..)

import ComplexNumbers
import Expect
import Field
import Fuzz
import HermitianMatrix exposing (HermitianMatrix)
import Matrix
import Quantum
import SquareMatrix exposing (SquareMatrix)
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
        , Test.test
            "tests expected value"
          <|
            \_ ->
                let
                    ket =
                        Quantum.Ket
                            (Vector.Vector
                                [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real (Basics.sqrt 2 / 2)) (ComplexNumbers.Imaginary 0)
                                , ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary (Basics.sqrt 2 / 2))
                                ]
                            )

                    hermitianMatrix =
                        Matrix.Matrix
                            [ Matrix.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.map negate ComplexNumbers.i ])
                            , Matrix.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.ComplexNumber (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0) ])
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> HermitianMatrix.HermitianMatrix
                in
                Quantum.expectedValue ket hermitianMatrix
                    |> Expect.equal (Result.Ok 2.5000000000000004)
        , Test.test
            "tests variance"
          <|
            \_ ->
                let
                    ket =
                        Quantum.Ket
                            (Vector.Vector
                                [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real (Basics.sqrt 2 / 2)) (ComplexNumbers.Imaginary 0)
                                , ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary (Basics.sqrt 2 / 2))
                                ]
                            )

                    hermitianMatrix =
                        Matrix.Matrix
                            [ Matrix.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.map negate ComplexNumbers.i ])
                            , Matrix.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.ComplexNumber (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0) ])
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> HermitianMatrix.HermitianMatrix
                in
                Quantum.variance ket hermitianMatrix
                    |> Expect.equal (Result.Ok 0.25)
        ]
