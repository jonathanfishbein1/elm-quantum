module Quantum exposing (..)

import Vector


type Ket a
    = Ket (Vector.Vector a)


ket0 : Ket Float
ket0 =
    Ket (Vector.Vector [ 1, 0 ])


ket1 : Ket Float
ket1 =
    Ket (Vector.Vector [ 0, 1 ])
