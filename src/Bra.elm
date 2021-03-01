module Bra exposing
    ( getAt
    , setAt
    , Bra(..)
    )

{-| Quantum Computing Simulator in Elm


# Manipulation

@docs getAt
@docs setAt

-}

import Matrix


{-| Bra Type
-}
type Bra a
    = Bra (Matrix.Matrix a)


{-| Get the value in a matrix at the specified row and column
-}
getAt : ( Int, Int ) -> Bra a -> Maybe a
getAt ( rowIndex, columnIndex ) (Bra matrix) =
    Matrix.getAt ( rowIndex, columnIndex ) matrix


{-| Set the value in a Matrix at the specified row and column
-}
setAt : ( Int, Int ) -> a -> Bra a -> Bra a
setAt ( rowIndex, columnIndex ) element (Bra matrix) =
    Matrix.setAt ( rowIndex, columnIndex ) element matrix
        |> Bra
