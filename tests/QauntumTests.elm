module QauntumTests exposing (..)

import ColumnVector
import ComplexNumbers
import Expect
import Field
import Fuzz
import HermitianMatrix
import Imaginary
import InvertableMatrix exposing (InvertableMatrix)
import Matrix
import NormalMatrix
import Quantum
import Real
import RowVector
import SquareMatrix
import SymmetricMatrix
import Test
import UnitaryMatrix exposing (UnitaryMatrix)
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
        , Test.test
            "tests and gate X^2 = I"
          <|
            \_ ->
                InvertableMatrix.multiply Vector.realInnerProductSpace Quantum.x Quantum.x
                    |> Expect.equal (Result.Ok (InvertableMatrix.identity Real.field 2))
        , Test.test
            "tests and gate sigmaX^2 = I"
          <|
            \_ ->
                UnitaryMatrix.multiply Quantum.sigmaX Quantum.sigmaX
                    |> Expect.equal (Result.Ok (UnitaryMatrix.identity 2))
        , Test.test
            "tests and gate sigmaY^2 = I"
          <|
            \_ ->
                UnitaryMatrix.multiply Quantum.sigmaY Quantum.sigmaY
                    |> Expect.equal (Result.Ok (UnitaryMatrix.identity 2))
        , Test.test
            "tests and gate sigmaZ^2 = I"
          <|
            \_ ->
                let
                    sigmaZSquared =
                        UnitaryMatrix.multiply Quantum.sigmaZ Quantum.sigmaZ
                            |> Result.withDefault Quantum.sigmaZ

                    i =
                        UnitaryMatrix.identity 2
                in
                Expect.true "sigmaZ^2 = I" (UnitaryMatrix.equal.eq sigmaZSquared i)
        , Test.test
            "tests H = 1/sqrt 2 (X + Z)"
          <|
            \_ ->
                let
                    xPlusZ =
                        InvertableMatrix.add Real.field Quantum.x Quantum.z
                            |> InvertableMatrix.scalarMultiplication Real.field (Real.Real (1 / Basics.sqrt 2))
                in
                Expect.equal Quantum.h xPlusZ
        ]
