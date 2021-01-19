module Quantum exposing
    ( add
    , h
    , ket0
    , ket1
    , ketMinus
    , ketPlus
    , scalarMultiplication
    , x
    )

import AbelianGroup
import Field
import Matrix
import Number.Bounded
import Vector


{-| Quantum Computing Simulator in Elm


# Types

@docs Ket


# Values

@docs ket0
@docs ket1
@docs ketPlus
@docs ketMinus


# Unitary Operations

@docs scalarMultiplication
@docs inverse


# Binary Operations

@docs add


# Quantum Operators

@docs h
@docs x

-}
type Ket a
    = Ket (Vector.Vector a)


probability : Number.Bounded.Bounded Float
probability =
    Number.Bounded.between 0 1


{-| Ket representing zero state
-}
ket0 : Ket (Number.Bounded.Bounded Float)
ket0 =
    Ket (Vector.Vector [ Number.Bounded.set 1 probability, Number.Bounded.set 0 probability ])


{-| Ket representing one state
-}
ket1 : Ket (Number.Bounded.Bounded Float)
ket1 =
    Ket (Vector.Vector [ Number.Bounded.set 0 probability, Number.Bounded.set 1 probability ])


{-| Ket representing + state
-}
ketPlus : Ket (Number.Bounded.Bounded Float)
ketPlus =
    add ket0 ket1
        |> scalarMultiplication (1 / Basics.sqrt 2)


{-| Ket representing + state
-}
ketMinus : Ket (Number.Bounded.Bounded Float)
ketMinus =
    add ket0 (inverse ket1)
        |> scalarMultiplication (1 / Basics.sqrt 2)


{-| Add two Kets
-}
add : Ket (Number.Bounded.Bounded Float) -> Ket (Number.Bounded.Bounded Float) -> Ket (Number.Bounded.Bounded Float)
add (Ket vectorOne) (Ket vectorTwo) =
    Vector.map2 (\vectOneNum vectTwoNum -> Number.Bounded.set (Number.Bounded.value vectOneNum + Number.Bounded.value vectTwoNum) probability) vectorOne vectorTwo
        |> Ket


{-| Multiply a Ket by a Scalar
-}
scalarMultiplication : Float -> Ket (Number.Bounded.Bounded Float) -> Ket (Number.Bounded.Bounded Float)
scalarMultiplication scalar (Ket vector) =
    Vector.scalarMultiplication Field.float scalar (Vector.map Number.Bounded.value vector)
        |> Vector.map (\element -> Number.Bounded.set element probability)
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
inverse : Ket (Number.Bounded.Bounded Float) -> Ket (Number.Bounded.Bounded Float)
inverse (Ket vector) =
    let
        (AbelianGroup.AbelianGroup vGroup) =
            Vector.realVectorSpace.abelianGroup
    in
    vGroup.inverse (Vector.map Number.Bounded.value vector)
        |> Vector.map (\element -> Number.Bounded.set element probability)
        |> Ket
