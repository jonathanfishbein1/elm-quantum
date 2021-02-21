module Quantum exposing
    ( Ket(..)
    , Bra(..)
    , ket0
    , ket1
    , ketPlus
    , ketMinus
    , ketComplex0
    , ketComplex1
    , ketComplexPlus
    , ketComplexMinus
    , scalarMultiplication
    , dimension
    , sum
    , foldl
    , variance
    , add
    , h
    , x
    , probabilityOfState
    , multiplyHermitianMatrixKet
    , expectedValue
    , varianceHermitianOperator
    , getAt
    , and, cNOT, equal, fredkin, multiplyInvertableMatrixKet, sigmaX, sigmaY, sigmaZ, toffoli
    )

{-| Quantum Computing Simulator in Elm


# Types

@docs Ket
@docs Bra


# Values

@docs ket0
@docs ket1
@docs ketPlus
@docs ketMinus
@docs ketComplex0
@docs ketComplex1
@docs ketComplexPlus
@docs ketComplexMinus


# Unitary Operations

@docs scalarMultiplication
@docs dimension
@docs sum
@docs foldl
@docs variance


# Binary Operations

@docs add


# Quantum Operators

@docs h
@docs x
@docs probabilityOfState
@docs multiplyHermitianMatrixKet
@docs expectedValue
@docs varianceHermitianOperator


# Manipulation

@docs getAt

-}

import AbelianGroup
import ColumnVector
import CommutativeDivisionRing
import ComplexNumbers
import Field
import Group
import HermitianMatrix
import Imaginary
import InvertableMatrix
import Matrix
import Monoid
import NormalMatrix
import Real
import RowVector
import SquareMatrix
import UnitaryMatrix
import Vector


{-| Ket Type
-}
type Ket a
    = Ket (ColumnVector.ColumnVector a)


{-| Bra Type
-}
type Bra a
    = Bra (Matrix.Matrix a)


{-| Calculate the probability of end state, the Bra, with given start state, the Ket
-}
probabilityOfState : Vector.InnerProductSpace a -> Ket a -> Bra a -> Result String a
probabilityOfState innerProductSpace (Ket kt) (Bra br) =
    let
        (Field.Field (CommutativeDivisionRing.CommutativeDivisionRing commutativeDivisionRing)) =
            innerProductSpace.vectorSpace.field

        (AbelianGroup.AbelianGroup group) =
            commutativeDivisionRing.addition
    in
    Matrix.multiplyMatrixVector innerProductSpace br kt
        |> Result.map (ColumnVector.sum group.monoid)


{-| Ket representing zero state
-}
ket0 : Ket (Real.Real Float)
ket0 =
    Vector.Vector
        [ Real.one
        , Real.zero
        ]
        |> ColumnVector.ColumnVector
        |> Ket


{-| Ket representing one state
-}
ket1 : Ket (Real.Real Float)
ket1 =
    Vector.Vector
        [ Real.zero
        , Real.one
        ]
        |> ColumnVector.ColumnVector
        |> Ket


{-| Ket representing + state
-}
ketPlus : Ket (Real.Real Float)
ketPlus =
    add Real.field ket0 ket1
        |> scalarMultiplication Real.field (Real.Real (1 / Basics.sqrt 2))


{-| Ket representing + state
-}
ketMinus : Ket (Real.Real Float)
ketMinus =
    add Real.field ket0 (inverse Real.sumGroup ket1)
        |> scalarMultiplication Real.field (Real.Real (1 / Basics.sqrt 2))


{-| Ket representing zero state with complex numbers
-}
ketComplex0 : Ket (ComplexNumbers.ComplexNumber Float)
ketComplex0 =
    Vector.Vector
        [ ComplexNumbers.one
        , ComplexNumbers.zero
        ]
        |> ColumnVector.ColumnVector
        |> Ket


{-| Ket representing one state with complex numbers
-}
ketComplex1 : Ket (ComplexNumbers.ComplexNumber Float)
ketComplex1 =
    Vector.Vector
        [ ComplexNumbers.zero
        , ComplexNumbers.one
        ]
        |> ColumnVector.ColumnVector
        |> Ket


{-| Ket representing + state with complex numbers
-}
ketComplexPlus : Ket (ComplexNumbers.ComplexNumber Float)
ketComplexPlus =
    add ComplexNumbers.field ketComplex0 ketComplex1
        |> scalarMultiplication ComplexNumbers.field (ComplexNumbers.ComplexNumber (Real.Real (1 / Basics.sqrt 2)) Imaginary.zero)


{-| Ket representing + state with complex numbers
-}
ketComplexMinus : Ket (ComplexNumbers.ComplexNumber Float)
ketComplexMinus =
    add ComplexNumbers.field ketComplex0 (inverse ComplexNumbers.sumGroup ketComplex1)
        |> scalarMultiplication ComplexNumbers.field (ComplexNumbers.ComplexNumber (Real.Real (1 / Basics.sqrt 2)) Imaginary.zero)


{-| Add two Kets
-}
add : Field.Field a -> Ket a -> Ket a -> Ket a
add field (Ket vectorOne) (Ket vectorTwo) =
    ColumnVector.add field vectorOne vectorTwo
        |> Ket


{-| Multiply a Ket by a Scalar
-}
scalarMultiplication : Field.Field a -> a -> Ket a -> Ket a
scalarMultiplication field scalar (Ket vector) =
    ColumnVector.scalarMultiplication field scalar vector
        |> Ket


{-| Hadamard Operation
-}
h : InvertableMatrix.InvertableMatrix (Real.Real Float)
h =
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ 1, 1 ])
        , RowVector.RowVector (Vector.Vector [ 1, -1 ])
        ]
        |> Matrix.map Real.Real
        |> Matrix.scalarMultiplication Real.field (Real.Real (1 / sqrt 2))
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| NOT Operation
-}
x : InvertableMatrix.InvertableMatrix (Real.Real Float)
x =
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ 0, 1 ])
        , RowVector.RowVector (Vector.Vector [ 1, 0 ])
        ]
        |> Matrix.map Real.Real
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| square root of NOT Operation
-}
squareRootNot : InvertableMatrix.InvertableMatrix (Real.Real Float)
squareRootNot =
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ 1, -1 ])
        , RowVector.RowVector (Vector.Vector [ 1, 1 ])
        ]
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
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.one ])
        , RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ])
        ]
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| SigmaX Operation
-}
sigmaY : UnitaryMatrix.UnitaryMatrix Float
sigmaY =
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.conjugate ComplexNumbers.i ])
        , RowVector.RowVector (Vector.Vector [ ComplexNumbers.i, ComplexNumbers.zero ])
        ]
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| SigmaX Operation
-}
sigmaZ : UnitaryMatrix.UnitaryMatrix Float
sigmaZ =
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ])
        , RowVector.RowVector (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.imaginaryAxisReflection ComplexNumbers.one ])
        ]
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix
        |> UnitaryMatrix.UnitaryMatrix


{-| controlled-NOT Operation
-}
cNOT : InvertableMatrix.InvertableMatrix (Real.Real Float)
cNOT =
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ 1, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 1, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 1 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 1, 0 ])
        ]
        |> Matrix.map Real.Real
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| Toffoli Operation
-}
toffoli : InvertableMatrix.InvertableMatrix (Real.Real Float)
toffoli =
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ 1, 0, 0, 0, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 1, 0, 0, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 1, 0, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 1, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 1, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 1, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 0, 1 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 1, 0 ])
        ]
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
    Matrix.Matrix
        [ RowVector.RowVector (Vector.Vector [ 1, 0, 0, 0, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 1, 0, 0, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 1, 0, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 1, 0, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 1, 0, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 1, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 1, 0, 0 ])
        , RowVector.RowVector (Vector.Vector [ 0, 0, 0, 0, 0, 0, 0, 1 ])
        ]
        |> Matrix.scalarMultiplication Field.float (1 / sqrt 2)
        |> SquareMatrix.SquareMatrix
        |> NormalMatrix.NormalMatrix
        |> InvertableMatrix.InvertableMatrix


{-| Inverse Ket
-}
inverse : Group.Group a -> Ket a -> Ket a
inverse group (Ket vector) =
    ColumnVector.map group.inverse vector
        |> Ket


{-| Get the value in a Ket at the specified index
-}
getAt : Int -> Ket a -> Maybe a
getAt index (Ket vector) =
    ColumnVector.getAt index vector


{-| Count of number of elements in a Ket
-}
dimension : Ket a -> Int
dimension (Ket vector) =
    ColumnVector.dimension vector


{-| Left fold over a Ket
-}
foldl : (a -> b -> b) -> b -> Ket a -> b
foldl foldFunction acc (Ket vector) =
    ColumnVector.foldl foldFunction acc vector


{-| Calculate the sum of a Ket
-}
sum : Monoid.Monoid a -> Ket a -> a
sum monoid (Ket vector) =
    ColumnVector.sum monoid vector


{-| Multiply a Vector by a Matrix
-}
multiplyHermitianMatrixKet :
    HermitianMatrix.HermitianMatrix Float
    -> Ket (ComplexNumbers.ComplexNumber Float)
    -> Result String (Ket (ComplexNumbers.ComplexNumber Float))
multiplyHermitianMatrixKet matrix (Ket vector) =
    HermitianMatrix.multiplyMatrixVector matrix vector
        |> Result.map Ket


{-| Multiply a Vector by a Matrix
-}
multiplyInvertableMatrixKet :
    Vector.InnerProductSpace a
    -> InvertableMatrix.InvertableMatrix a
    -> Ket a
    -> Result String (Ket a)
multiplyInvertableMatrixKet innerProductSpace matrix (Ket vector) =
    InvertableMatrix.multiplyMatrixVector innerProductSpace matrix vector
        |> Result.map Ket


{-| Calculate the expected value when a Ket is multiplied by a Hermitian Matrix
-}
expectedValue :
    Ket (ComplexNumbers.ComplexNumber Float)
    -> HermitianMatrix.HermitianMatrix Float
    -> Result String (Real.Real Float)
expectedValue ket matrix =
    multiplyHermitianMatrixKet matrix ket
        |> Result.map (conjugate >> (\(Ket (ColumnVector.ColumnVector v)) -> Bra (Matrix.Matrix [ RowVector.RowVector v ])))
        |> Result.andThen (probabilityOfState Vector.complexInnerProductSpace ket)
        |> Result.map ComplexNumbers.real


{-| Map over a vector
-}
map : (a -> b) -> Ket a -> Ket b
map f (Ket vector) =
    Ket <| ColumnVector.map f vector


{-| Take the complex conjugate of a Complex Numbered Vector
-}
conjugate :
    Ket (ComplexNumbers.ComplexNumber number)
    -> Ket (ComplexNumbers.ComplexNumber number)
conjugate (Ket vector) =
    ColumnVector.conjugate vector
        |> Ket


{-| Compare two vectors for equality using a comparator
-}
equal : (a -> a -> Bool) -> Ket a -> Ket a -> Bool
equal comparator (Ket vectorOne) (Ket vectorTwo) =
    (ColumnVector.equal comparator).eq vectorOne vectorTwo


varianceHermitianOperator : Ket (ComplexNumbers.ComplexNumber Float) -> HermitianMatrix.HermitianMatrix Float -> Result String (HermitianMatrix.HermitianMatrix Float)
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


variance : Ket (ComplexNumbers.ComplexNumber Float) -> HermitianMatrix.HermitianMatrix Float -> Result String (Real.Real Float)
variance ket matrix =
    Result.andThen (expectedValue ket) (varianceHermitianOperator ket matrix)
