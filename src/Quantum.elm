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
    , add
    , h
    , x
    , probabilityOfState
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


# Binary Operations

@docs add


# Quantum Operators

@docs h
@docs x
@docs probabilityOfState

-}

import AbelianGroup
import CommutativeDivisionRing
import ComplexNumbers
import Field
import Group
import Matrix
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
probabilityOfState : Vector.InnerProductSpace a -> Bra a -> Ket a -> Result String a
probabilityOfState innerProductSpace (Bra br) (Ket kt) =
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


{-| Dimension of Ket
-}
dimension : Ket a -> Int
dimension (Ket vector) =
    Vector.dimension vector
