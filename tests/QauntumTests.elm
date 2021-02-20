module QauntumTests exposing (..)

import ColumnVector
import ComplexNumbers
import Expect
import Field
import Fuzz
import HermitianMatrix
import Imaginary
import InvertableMatrix
import Matrix
import NormalMatrix
import Quantum
import Real
import RowVector
import SquareMatrix
import SymmetricMatrix
import Test
import Vector


boolToInt : Bool -> Int
boolToInt bool =
    if bool == True then
        1

    else
        0


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
                                    [ ComplexNumbers.ComplexNumber (Real.Real (Basics.sqrt 2 / 2)) Imaginary.zero
                                    , ComplexNumbers.ComplexNumber Real.zero (Imaginary.Imaginary (Real.Real (Basics.sqrt 2 / 2)))
                                    ]
                                )
                            )

                    hermitianMatrix =
                        Matrix.Matrix
                            [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.map negate ComplexNumbers.i ])
                            , RowVector.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.ComplexNumber (Real.Real 2) Imaginary.zero ])
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> SymmetricMatrix.SymmetricMatrix
                            |> HermitianMatrix.HermitianMatrix
                in
                Quantum.expectedValue ket hermitianMatrix
                    |> Expect.equal (Result.Ok (Real.Real 2.5000000000000004))
        , Test.test
            "tests variance"
          <|
            \_ ->
                let
                    ket =
                        Quantum.Ket
                            (ColumnVector.ColumnVector
                                (Vector.Vector
                                    [ ComplexNumbers.ComplexNumber (Real.Real (Basics.sqrt 2 / 2)) Imaginary.zero
                                    , ComplexNumbers.ComplexNumber Real.zero (Imaginary.Imaginary (Real.Real (Basics.sqrt 2 / 2)))
                                    ]
                                )
                            )

                    hermitianMatrix =
                        Matrix.Matrix
                            [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.map negate ComplexNumbers.i ])
                            , RowVector.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.ComplexNumber (Real.Real 2) Imaginary.zero ])
                            ]
                            |> SquareMatrix.SquareMatrix
                            |> NormalMatrix.NormalMatrix
                            |> SymmetricMatrix.SymmetricMatrix
                            |> HermitianMatrix.HermitianMatrix
                in
                Quantum.variance ket hermitianMatrix
                    |> Expect.equal (Result.Ok (Real.Real 0.25))
        , Test.fuzz
            Fuzz.bool
            "tests Not gate"
          <|
            \valOne ->
                let
                    ket =
                        Quantum.Ket
                            (ColumnVector.ColumnVector
                                (Vector.Vector
                                    [ boolToInt valOne
                                        |> toFloat
                                        |> Real.Real
                                    , boolToInt (not valOne)
                                        |> toFloat
                                        |> Real.Real
                                    ]
                                )
                            )

                    expected =
                        Quantum.Ket
                            (ColumnVector.ColumnVector
                                (Vector.Vector
                                    [ boolToInt (not valOne)
                                        |> toFloat
                                        |> Real.Real
                                    , boolToInt valOne
                                        |> toFloat
                                        |> Real.Real
                                    ]
                                )
                            )
                in
                Quantum.multiplyInvertableMatrixKet Vector.realInnerProductSpace Quantum.x ket
                    |> Expect.equal (Result.Ok expected)
        , Test.test
            "tests Not gate 00"
          <|
            \_ ->
                let
                    ket =
                        Vector.Vector
                            [ 1
                            , 0
                            , 0
                            , 0
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket

                    expectedKet =
                        Vector.Vector
                            [ Real.one
                            , Real.zero
                            , Real.zero
                            , Real.zero
                            ]
                            |> ColumnVector.ColumnVector
                            |> Quantum.Ket
                in
                Quantum.multiplyInvertableMatrixKet Vector.realInnerProductSpace Quantum.cNOT ket
                    |> Expect.equal (Result.Ok expectedKet)
        , Test.test
            "tests Not gate 01"
          <|
            \_ ->
                let
                    ket =
                        Vector.Vector
                            [ 0
                            , 1
                            , 0
                            , 0
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket

                    expectedKet =
                        Vector.Vector
                            [ 0
                            , 1
                            , 0
                            , 0
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket
                in
                Quantum.multiplyInvertableMatrixKet Vector.realInnerProductSpace Quantum.cNOT ket
                    |> Expect.equal (Result.Ok expectedKet)
        , Test.test
            "tests Not gate 10"
          <|
            \_ ->
                let
                    ket =
                        Vector.Vector
                            [ 0
                            , 0
                            , 1
                            , 0
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket

                    expectedKet =
                        Vector.Vector
                            [ 0
                            , 0
                            , 0
                            , 1
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket
                in
                Quantum.multiplyInvertableMatrixKet Vector.realInnerProductSpace Quantum.cNOT ket
                    |> Expect.equal (Result.Ok expectedKet)
        , Test.test
            "tests Not gate 11"
          <|
            \_ ->
                let
                    ket =
                        Vector.Vector
                            [ 0
                            , 0
                            , 0
                            , 1
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket

                    expectedKet =
                        Vector.Vector
                            [ 0
                            , 0
                            , 1
                            , 0
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket
                in
                Quantum.multiplyInvertableMatrixKet Vector.realInnerProductSpace Quantum.cNOT ket
                    |> Expect.equal (Result.Ok expectedKet)
        , Test.test
            "tests and gate 000"
          <|
            \_ ->
                let
                    ket =
                        Vector.Vector
                            [ 1
                            , 0
                            , 0
                            , 0
                            , 0
                            , 0
                            , 0
                            , 0
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket

                    expectedKet =
                        Vector.Vector
                            [ 1
                            , 0
                            , 0
                            , 0
                            , 0
                            , 0
                            , 0
                            , 0
                            ]
                            |> ColumnVector.ColumnVector
                            |> ColumnVector.map Real.Real
                            |> Quantum.Ket
                in
                Quantum.multiplyInvertableMatrixKet Vector.realInnerProductSpace Quantum.and ket
                    |> Expect.equal (Result.Ok expectedKet)
        ]
