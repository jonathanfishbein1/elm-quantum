module Quantum exposing (..)

import Matrix
import Vector


type Ket a
    = Ket (Vector.Vector a)


ket0 : Ket Float
ket0 =
    Ket (Vector.Vector [ 1, 0 ])


ket1 : Ket Float
ket1 =
    Ket (Vector.Vector [ 0, 1 ])


h : Matrix.Matrix Float
h =
    Matrix.Matrix
        [ Matrix.RowVector (Vector.Vector [ 1, 1 ])
        , Matrix.RowVector (Vector.Vector [ 1, -1 ])
        ]
