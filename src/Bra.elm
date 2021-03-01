module Bra exposing (Bra(..))

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


# Equality

@docs equal


# Manipulation

@docs getAt
@docs setAt

-}

import Matrix


{-| Bra Type
-}
type Bra a
    = Bra (Matrix.Matrix a)
