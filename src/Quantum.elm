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
    , add
    , h
    , x
    , probabilityOfState
    , multiplyHermitianMatrixKet
    , expectedValue
    , varianceHermitianOperator
    , getAt
    , equal
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
import CommutativeDivisionRing
import ComplexNumbers
import Field
import Group
import HermitianMatrix exposing (HermitianMatrix(..))
import Matrix
import Monoid
import SquareMatrix
import Vector


{-| Ket Type
-}
type Ket a
    = Ket (Vector.Vector a)


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
        |> Result.map (Vector.sum group.monoid)


{-| Ket representing zero state
-}
ket0 : Ket Float
ket0 =
    Ket (Vector.Vector [ 1, 0 ])


{-| Ket representing one state
-}
ket1 : Ket Float
ket1 =
    Ket (Vector.Vector [ 0, 1 ])


{-| Ket representing + state
-}
ketPlus : Ket Float
ketPlus =
    add Field.float ket0 ket1
        |> scalarMultiplication Field.float (1 / Basics.sqrt 2)


{-| Ket representing + state
-}
ketMinus : Ket Float
ketMinus =
    add Field.float ket0 (inverse Group.numberSum ket1)
        |> scalarMultiplication Field.float (1 / Basics.sqrt 2)


{-| Ket representing zero state with complex numbers
-}
ketComplex0 : Ket (ComplexNumbers.ComplexNumber Float)
ketComplex0 =
    Ket (Vector.Vector [ ComplexNumbers.one, ComplexNumbers.zero ])


{-| Ket representing one state with complex numbers
-}
ketComplex1 : Ket (ComplexNumbers.ComplexNumber Float)
ketComplex1 =
    Ket (Vector.Vector [ ComplexNumbers.zero, ComplexNumbers.one ])


{-| Ket representing + state with complex numbers
-}
ketComplexPlus : Ket (ComplexNumbers.ComplexNumber Float)
ketComplexPlus =
    add ComplexNumbers.complexField ketComplex0 ketComplex1
        |> scalarMultiplication ComplexNumbers.complexField (ComplexNumbers.ComplexNumber (ComplexNumbers.Real (1 / Basics.sqrt 2)) (ComplexNumbers.Imaginary 0))


{-| Ket representing + state with complex numbers
-}
ketComplexMinus : Ket (ComplexNumbers.ComplexNumber Float)
ketComplexMinus =
    add ComplexNumbers.complexField ketComplex0 (inverse ComplexNumbers.complexSumGroup ketComplex1)
        |> scalarMultiplication ComplexNumbers.complexField (ComplexNumbers.ComplexNumber (ComplexNumbers.Real (1 / Basics.sqrt 2)) (ComplexNumbers.Imaginary 0))


{-| Add two Kets
-}
add : Field.Field a -> Ket a -> Ket a -> Ket a
add field (Ket vectorOne) (Ket vectorTwo) =
    Vector.add field vectorOne vectorTwo
        |> Ket


{-| Multiply a Ket by a Scalar
-}
scalarMultiplication : Field.Field a -> a -> Ket a -> Ket a
scalarMultiplication field scalar (Ket vector) =
    Vector.scalarMultiplication field scalar vector
        |> Ket


{-| Hadamard Operation
-}
h : Matrix.Matrix Float
h =
    Matrix.Matrix
        [ Matrix.RowVector (Vector.Vector [ 1, 1 ])
        , Matrix.RowVector (Vector.Vector [ 1, -1 ])
        ]
        |> Matrix.scalarMultiplication Field.float (1 / sqrt 2)


{-| NOT Operation
-}
x : Matrix.Matrix Float
x =
    Matrix.Matrix
        [ Matrix.RowVector (Vector.Vector [ 0, 1 ])
        , Matrix.RowVector (Vector.Vector [ 1, 0 ])
        ]
        |> Matrix.scalarMultiplication Field.float (1 / sqrt 2)


{-| Inverse Ket
-}
inverse : Group.Group a -> Ket a -> Ket a
inverse group (Ket vector) =
    Vector.map group.inverse vector
        |> Ket


{-| Get the value in a Ket at the specified index
-}
getAt : Int -> Ket a -> Maybe a
getAt index (Ket vector) =
    Vector.getAt index vector


{-| Count of number of elements in a Ket
-}
dimension : Ket a -> Int
dimension (Ket vector) =
    Vector.dimension vector


{-| Left fold over a Ket
-}
foldl : (a -> b -> b) -> b -> Ket a -> b
foldl foldFunction acc (Ket vector) =
    Vector.foldl foldFunction acc vector


{-| Calculate the sum of a Ket
-}
sum : Monoid.Monoid a -> Ket a -> a
sum monoid (Ket vector) =
    Vector.sum monoid vector


{-| Multiply a Vector by a Matrix
-}
multiplyHermitianMatrixKet :
    HermitianMatrix.HermitianMatrix Float
    -> Ket (ComplexNumbers.ComplexNumber Float)
    -> Result String (Ket (ComplexNumbers.ComplexNumber Float))
multiplyHermitianMatrixKet matrix (Ket vector) =
    HermitianMatrix.multiplyMatrixVector matrix vector
        |> Result.map Ket


{-| Calculate the expected value when a Ket is multiplied by a Hermitian Matrix
-}
expectedValue :
    HermitianMatrix.HermitianMatrix Float
    -> Ket (ComplexNumbers.ComplexNumber Float)
    -> Result String Float
expectedValue matrix ket =
    multiplyHermitianMatrixKet matrix ket
        |> Result.map (conjugate >> (\(Ket v) -> Bra (Matrix.Matrix [ Matrix.RowVector v ])))
        |> Result.andThen (probabilityOfState Vector.complexInnerProductSpace ket)
        |> Result.map ComplexNumbers.real


{-| Map over a vector
-}
map : (a -> b) -> Ket a -> Ket b
map f (Ket vector) =
    Ket <| Vector.map f vector


{-| Take the complex conjugate of a Complex Numbered Vector
-}
conjugate :
    Ket (ComplexNumbers.ComplexNumber number)
    -> Ket (ComplexNumbers.ComplexNumber number)
conjugate =
    map ComplexNumbers.conjugate


{-| Compare two vectors for equality using a comparator
-}
equal : (a -> a -> Bool) -> Ket a -> Ket a -> Bool
equal comparator (Ket vectorOne) (Ket vectorTwo) =
    Vector.equal comparator vectorOne vectorTwo


varianceHermitianOperator : Ket (ComplexNumbers.ComplexNumber Float) -> HermitianMatrix.HermitianMatrix Float -> Result String (HermitianMatrix.HermitianMatrix Float)
varianceHermitianOperator ket matrix =
    let
        identityM =
            HermitianMatrix.dimension matrix
                |> HermitianMatrix.identity
    in
    expectedValue matrix ket
        |> Result.map
            ((\extVal -> HermitianMatrix.scalarMultiplication (ComplexNumbers.ComplexNumber (ComplexNumbers.Real extVal) (ComplexNumbers.Imaginary 0)) identityM)
                >> HermitianMatrix.subtract matrix
            )
        |> Result.andThen (\dif -> HermitianMatrix.multiply dif dif)
