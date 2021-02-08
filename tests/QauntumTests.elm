module QauntumTests exposing (..)

import ColumnVector
import ComplexNumbers
import Expect
import Field
import Fuzz
import HermitianMatrix
import Matrix
import NormalMatrix
import Quantum
import RowVector
import SquareMatrix
import SymmetricMatrix
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
                            (ColumnVector.ColumnVector
                                (Vector.Vector
                                    [ one
                                    , two
                                    ]
                                )
                            )

                    w =
                        Quantum.Ket
                            (ColumnVector.ColumnVector
                                (Vector.Vector
                                    [ one
                                    , two
                                    ]
                                )
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
                            (ColumnVector.ColumnVector
                                (Vector.Vector
                                    [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real (Basics.sqrt 2 / 2)) (ComplexNumbers.Imaginary 0)
                                    , ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary (Basics.sqrt 2 / 2))
                                    ]
                                )
                            )

                    hermitianMatrix =
                        Matrix.Matrix
                            [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.map negate ComplexNumbers.i ])
                            , RowVector.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.ComplexNumber (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0) ])
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> SymmetricMatrix.SymmetricMatrix
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
                            (ColumnVector.ColumnVector
                                (Vector.Vector
                                    [ ComplexNumbers.ComplexNumber (ComplexNumbers.Real (Basics.sqrt 2 / 2)) (ComplexNumbers.Imaginary 0)
                                    , ComplexNumbers.ComplexNumber (ComplexNumbers.Real 0) (ComplexNumbers.Imaginary (Basics.sqrt 2 / 2))
                                    ]
                                )
                            )

                    hermitianMatrix =
                        Matrix.Matrix
                            [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.map negate ComplexNumbers.i ])
                            , RowVector.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.ComplexNumber (ComplexNumbers.Real 2) (ComplexNumbers.Imaginary 0) ])
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> SymmetricMatrix.SymmetricMatrix
                            |> HermitianMatrix.HermitianMatrix
                in
                Quantum.variance ket hermitianMatrix
                    |> Expect.equal (Result.Ok 0.25)
        ]
