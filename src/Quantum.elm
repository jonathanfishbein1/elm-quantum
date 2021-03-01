module Quantum exposing
    ( variance
    , multiplyInvertableMatrixKet
    , h
    , hComplex
    , x
    , squareRootNot
    , sigmaXReal
    , sigmaX
    , sigmaY
    , sigmaZ
    , z
    , toffoli
    , fredkin
    , t
    , s
    , cNOT
    , and
    , probabilityOfState
    , multiplyHermitianMatrixKet
    , expectedValue
    , varianceHermitianOperator
    )

{-| Quantum Computing Simulator in Elm


# Types


# Values

@docs ket0
@docs ket1
@docs ketPlus
@docs ketMinus
@docs ketComplex0
@docs ketComplex1
@docs ketComplexPlus
@docs ketComplexMinus
@docs ketEmpty


# Unitary Operations

@docs scalarMultiplication
@docs dimension
@docs sum
@docs foldl
@docs variance
@docs map
@docs lengthReal
@docs lengthComplex
@docs normaliseReal
@docs normaliseComplex


# Binary Operations

@docs add
@docs multiplyInvertableMatrixKet


# Quantum Operators

@docs h
@docs hComplex
@docs x
@docs squareRootNot
@docs sigmaXReal
@docs sigmaX
@docs sigmaY
@docs sigmaZ
@docs z
@docs toffoli
@docs fredkin
@docs t
@docs s
@docs cNOT
@docs and
@docs probabilityOfState
@docs multiplyHermitianMatrixKet
@docs expectedValue
@docs varianceHermitianOperator


# Equality

@docs equal


# Manipulation

@docs getAt
@docs setAt

-}

import AbelianGroup
import Bra
import ColumnVector
import CommutativeDivisionRing
import ComplexNumbers
import Field
import HermitianMatrix
import Imaginary
import InvertableMatrix
import Ket
import Matrix
import NormalMatrix
import Real
import RowVector
import SquareMatrix
import UnitaryMatrix
import Vector


{-| Calculate the probability of end state, the Bra.Bra, with given start state, the Ket.Ket
-}
probabilityOfState : RowVector.InnerProductSpace a -> Ket.Ket a -> Bra.Bra a -> Result String a
probabilityOfState innerProductSpace (Ket.Ket kt) (Bra.Bra br) =
    let
        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            innerProductSpace.vectorSpace.field

        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    Matrix.multiplyMatrixVector innerProductSpace br kt
        |> Result.map (ColumnVector.sum group.monoid)


{-| Hadamard Operation
-}
h : InvertableMatrix.InvertableMatrix (Real.Real Float)
h =
    [ RowVector.RowVector (Vector.Vector [ 1, 1 ])
    , RowVector.RowVector (Vector.Vector [ 1, -1 ])
    ]
        |> Matrix.Matrix
        |> Matrix.map Real.Real
        |> Matrix.scalarMultiplication Real.field (Real.Real (1 / sqrt 2))
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| Hadamard Operation
-}
hComplex : UnitaryMatrix.UnitaryMatrix Float
hComplex =
    [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.one ])
    , RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.negativeOne ])
    ]
        |> Matrix.Matrix
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix
        |> UnitaryMatrix.scalarMultiplication (ComplexNumbers.ComplexNumber (Real.Real (1 / sqrt 2)) Imaginary.zero)


{-| NOT Operation
-}
x : InvertableMatrix.InvertableMatrix (Real.Real Float)
x =
    [ RowVector.RowVector (Vector.Vector [ 0, 1 ])
    , RowVector.RowVector (Vector.Vector [ 1, 0 ])
    ]
        |> Matrix.Matrix
        |> Matrix.map Real.Real
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| z Operation
-}
z : InvertableMatrix.InvertableMatrix (Real.Real Float)
z =
    [ RowVector.RowVector (Vector.Vector [ 1, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, -1 ])
    ]
        |> Matrix.Matrix
        |> Matrix.map Real.Real
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| square root of NOT Operation
-}
squareRootNot : InvertableMatrix.InvertableMatrix (Real.Real Float)
squareRootNot =
    [ RowVector.RowVector (Vector.Vector [ 1, -1 ])
    , RowVector.RowVector (Vector.Vector [ 1, 1 ])
    ]
        |> Matrix.Matrix
        |> Matrix.map Real.Real
        |> Matrix.scalarMultiplication Real.field (Real.Real (1 / sqrt 2))
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| SigmaX Operation
-}
sigmaXReal : InvertableMatrix.InvertableMatrix (Real.Real Float)
sigmaXReal =
    x


{-| SigmaX Operation
-}
sigmaX : UnitaryMatrix.UnitaryMatrix Float
sigmaX =
    [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.one ])
    , RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ])
    ]
        |> Matrix.Matrix
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| SigmaX Operation
-}
sigmaY : UnitaryMatrix.UnitaryMatrix Float
sigmaY =
    [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.negativeI ])
    , RowVector.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.zero ])
    ]
        |> Matrix.Matrix
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| SigmaX Operation
-}
sigmaZ : UnitaryMatrix.UnitaryMatrix Float
sigmaZ =
    [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ])
    , RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.negativeOne ])
    ]
        |> Matrix.Matrix
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| s Operation
-}
s : UnitaryMatrix.UnitaryMatrix Float
s =
    [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ])
    , RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.i ])
    ]
        |> Matrix.Matrix
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| t Operation
-}
t : UnitaryMatrix.UnitaryMatrix Float
t =
    [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ])
    , RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.eToTheIPiOver4 ])
    ]
        |> Matrix.Matrix
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| controlled-NOT Operation
-}
cNOT : InvertableMatrix.InvertableMatrix (Real.Real Float)
cNOT =
    [ RowVector.RowVector (Vector.Vector [ 1, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 1, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 1 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 1, 0 ])
    ]
        |> Matrix.Matrix
        |> Matrix.map Real.Real
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| Toffoli Operation
-}
toffoli : InvertableMatrix.InvertableMatrix (Real.Real Float)
toffoli =
    [ RowVector.RowVector (Vector.Vector [ 1, 0, 0, 0, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 1, 0, 0, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 1, 0, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 1, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 1, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 1, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 0, 1 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 1, 0 ])
    ]
        |> Matrix.Matrix
        |> Matrix.map Real.Real
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| and Operation
-}
and : InvertableMatrix.InvertableMatrix (Real.Real Float)
and =
    toffoli


{-| Fredkin Operation
-}
fredkin : InvertableMatrix.InvertableMatrix Float
fredkin =
    [ RowVector.RowVector (Vector.Vector [ 1, 0, 0, 0, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 1, 0, 0, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 1, 0, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 1, 0, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 1, 0, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 1, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 1, 0, 0 ])
    , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 0, 1 ])
    ]
        |> Matrix.Matrix
        |> Matrix.scalarMultiplication Field.float (1 / sqrt 2)
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| Multiply a Vector by a Matrix
-}
multiplyHermitianMatrixKet :
    HermitianMatrix.HermitianMatrix Float
    -> Ket.Ket (ComplexNumbers.ComplexNumber Float)
    -> Result String (Ket.Ket (ComplexNumbers.ComplexNumber Float))
multiplyHermitianMatrixKet matrix (Ket.Ket vector) =
    HermitianMatrix.multiplyMatrixVector matrix vector
        |> Result.map Ket.Ket


{-| Multiply a Vector by a Matrix
-}
multiplyInvertableMatrixKet :
    RowVector.InnerProductSpace a
    -> InvertableMatrix.InvertableMatrix a
    -> Ket.Ket a
    -> Result String (Ket.Ket a)
multiplyInvertableMatrixKet innerProductSpace matrix (Ket.Ket vector) =
    InvertableMatrix.multiplyMatrixVector innerProductSpace matrix vector
        |> Result.map Ket.Ket


{-| Calculate the expected value when a Ket.Ket is multiplied by a Hermitian Matrix
-}
expectedValue :
    Ket.Ket (ComplexNumbers.ComplexNumber Float)
    -> HermitianMatrix.HermitianMatrix Float
    -> Result String (Real.Real Float)
expectedValue ket matrix =
    multiplyHermitianMatrixKet matrix ket
        |> Result.map (Ket.conjugate >> (\(Ket.Ket (ColumnVector.ColumnVector v)) -> Bra.Bra (Matrix.Matrix [ RowVector.RowVector v ])))
        |> Result.andThen (probabilityOfState RowVector.complexInnerProductSpace ket)
        |> Result.map ComplexNumbers.real


{-| varianceHermitianOperator
-}
varianceHermitianOperator : Ket.Ket (ComplexNumbers.ComplexNumber Float) -> HermitianMatrix.HermitianMatrix Float -> Result String (HermitianMatrix.HermitianMatrix Float)
varianceHermitianOperator ket matrix =
    let
        identityM =
            HermitianMatrix.dimension matrix
                |> HermitianMatrix.identity
    in
    expectedValue ket matrix
        |> Result.map
            ((\(Real.Real extVal) -> HermitianMatrix.scalarMultiplication (ComplexNumbers.ComplexNumber (Real.Real extVal) Imaginary.zero) identityM)
                >> HermitianMatrix.subtract matrix
            )
        |> Result.andThen (\dif -> HermitianMatrix.multiply dif dif)


{-| variance
-}
variance : Ket.Ket (ComplexNumbers.ComplexNumber Float) -> HermitianMatrix.HermitianMatrix Float -> Result String (Real.Real Float)
variance ket matrix =
    Result.andThen (expectedValue ket) (varianceHermitianOperator ket matrix)
