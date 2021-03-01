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
    , ketEmpty
    , scalarMultiplication
    , dimension
    , sum
    , foldl
    , variance
    , map
    , lengthReal
    , lengthComplex
    , normaliseReal
    , normaliseComplex
    , add
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
    , equal
    , getAt
    , setAt
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
import ColumnVector
import CommutativeDivisionRing
import CommutativeMonoid
import CommutativeSemigroup
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
import Semigroup
import SquareMatrix
import UnitaryMatrix
import Vector


{-| Ket Type
-}
type Ket a
    = Ket (ColumnVector.ColumnVector a)


{-| Type to represent a Vector Space
-}
type alias VectorSpace a =
    { abelianGroup : AbelianGroup.AbelianGroup (Ket a)
    , vectorScalarMultiplication : a -> Ket a -> Ket a
    , field : Field.Field a
    }


{-| Type to represent an Inner Product Space
-}
type alias InnerProductSpace a =
    { vectorSpace : VectorSpace a
    , innerProduct : Ket a -> Ket a -> a
    , length : Ket a -> Real.Real Float
    , distance : Ket a -> Ket a -> Real.Real Float
    }


{-| Bra Type
-}
type Bra a
    = Bra (Matrix.Matrix a)


{-| Calculate the probability of end state, the Bra, with given start state, the Ket
-}
probabilityOfState : RowVector.InnerProductSpace a -> Ket a -> Bra a -> Result String a
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
    [ Real.one
    , Real.zero
    ]
        |> Vector.Vector
        |> ColumnVector.ColumnVector
        |> Ket


{-| Ket representing one state
-}
ket1 : Ket (Real.Real Float)
ket1 =
    [ Real.zero
    , Real.one
    ]
        |> Vector.Vector
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
    [ ComplexNumbers.one
    , ComplexNumbers.zero
    ]
        |> Vector.Vector
        |> ColumnVector.ColumnVector
        |> Ket


{-| Ket representing one state with complex numbers
-}
ketComplex1 : Ket (ComplexNumbers.ComplexNumber Float)
ketComplex1 =
    [ ComplexNumbers.zero
    , ComplexNumbers.one
    ]
        |> Vector.Vector
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


{-| Empty ket
-}
ketEmpty : Ket a
ketEmpty =
    ColumnVector.empty
        |> Ket


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


{-| Set the value in a Ket at the specified index
-}
setAt : Int -> a -> Ket a -> Ket a
setAt index element (Ket list) =
    ColumnVector.setAt index element list
        |> Ket


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
    RowVector.InnerProductSpace a
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
        |> Result.andThen (probabilityOfState RowVector.complexInnerProductSpace ket)
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


{-| varianceHermitianOperator
-}
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


{-| variance
-}
variance : Ket (ComplexNumbers.ComplexNumber Float) -> HermitianMatrix.HermitianMatrix Float -> Result String (Real.Real Float)
variance ket matrix =
    Result.andThen (expectedValue ket) (varianceHermitianOperator ket matrix)


{-| Calculate the dot product of two Vectors
-}
dotProduct : Field.Field a -> Ket a -> Ket a -> a
dotProduct field (Ket vectorOne) (Ket vectorTwo) =
    ColumnVector.dotProduct field vectorOne vectorTwo


{-| Calculate distance between two vectors
-}
distanceReal : Ket (Real.Real Float) -> Ket (Real.Real Float) -> Real.Real Float
distanceReal (Ket vectorOne) (Ket vectorTwo) =
    ColumnVector.distanceReal vectorOne vectorTwo


{-| Calculate distance between two vectors
-}
distanceComplex : Ket (ComplexNumbers.ComplexNumber Float) -> Ket (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
distanceComplex (Ket vectorOne) (Ket vectorTwo) =
    ColumnVector.distanceComplex vectorOne vectorTwo


{-| Calculate the length of a Real valued Vector
-}
lengthReal : Ket (Real.Real Float) -> Real.Real Float
lengthReal (Ket vector) =
    ColumnVector.lengthReal vector


{-| Calculate the length of a Complex valued Vector
-}
lengthComplex : Ket (ComplexNumbers.ComplexNumber Float) -> Real.Real Float
lengthComplex (Ket vector) =
    ColumnVector.lengthComplex vector


{-| Adjust a real valued column vector so that its length is exactly one
-}
normaliseReal : Ket (Real.Real Float) -> Ket (Real.Real Float)
normaliseReal (Ket v) =
    ColumnVector.normaliseReal v
        |> Ket


{-| Adjust a complex valued column vector so that its length is exactly one
-}
normaliseComplex : Ket (ComplexNumbers.ComplexNumber Float) -> Ket (ComplexNumbers.ComplexNumber Float)
normaliseComplex (Ket v) =
    ColumnVector.normaliseComplex v
        |> Ket


{-| Semigroup instance for a real valued Internal.Vector.
-}
realSemigroup : Semigroup.Semigroup (Ket (Real.Real Float))
realSemigroup =
    add Real.field


{-| Semigroup instance for a complex valued Internal.Vector.
-}
complexSemigroup : Semigroup.Semigroup (Ket (ComplexNumbers.ComplexNumber Float))
complexSemigroup =
    add ComplexNumbers.field


{-| Commutative Semigroup instance for a real valued Internal.Vector.
-}
realCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Ket (Real.Real Float))
realCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup realSemigroup


{-| Commutative Semigroup instance for a complex valued Internal.Vector.
-}
complexCommutativeSemigroup : CommutativeSemigroup.CommutativeSemigroup (Ket (ComplexNumbers.ComplexNumber Float))
complexCommutativeSemigroup =
    CommutativeSemigroup.CommutativeSemigroup complexSemigroup


{-| Monoid instance for a real valued Internal.Vector.
-}
realMonoid : Monoid.Monoid (Ket (Real.Real Float))
realMonoid =
    Monoid.semigroupAndIdentity realSemigroup ketEmpty


{-| Monoid instance for a complex valued Internal.Vector.
-}
complexMonoid : Monoid.Monoid (Ket (ComplexNumbers.ComplexNumber Float))
complexMonoid =
    Monoid.semigroupAndIdentity complexSemigroup ketEmpty


{-| Commutative Monoid instance for a real valued Internal.Vector.
-}
realCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Ket (Real.Real Float))
realCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid realMonoid


{-| Commutative Monoid instance for a complex valued Internal.Vector.
-}
complexCommutativeMonoid : CommutativeMonoid.CommutativeMonoid (Ket (ComplexNumbers.ComplexNumber Float))
complexCommutativeMonoid =
    CommutativeMonoid.CommutativeMonoid complexMonoid


{-| Group instance for a real valued Internal.Vector.
-}
realGroup : Group.Group (Ket (Real.Real Float))
realGroup =
    { monoid = realMonoid
    , inverse = map Real.sumGroup.inverse
    }


{-| Group instance for a complex valued Internal.Vector.
-}
complexGroup : Group.Group (Ket (ComplexNumbers.ComplexNumber Float))
complexGroup =
    { monoid = complexMonoid
    , inverse = map ComplexNumbers.sumGroup.inverse
    }


{-| Abelian Group instance for a real valued Internal.Vector.
-}
realAbelianGroup : AbelianGroup.AbelianGroup (Ket (Real.Real Float))
realAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = realMonoid
        , inverse = realGroup.inverse
        }


{-| Group instance for a complex valued Internal.Vector.
-}
complexAbelianGroup : AbelianGroup.AbelianGroup (Ket (ComplexNumbers.ComplexNumber Float))
complexAbelianGroup =
    AbelianGroup.AbelianGroup
        { monoid = complexMonoid
        , inverse = complexGroup.inverse
        }


{-| Real Numbered Vector Space
-}
realVectorSpace : VectorSpace (Real.Real Float)
realVectorSpace =
    { abelianGroup = realAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication Real.field
    , field = Real.field
    }


{-| Complex Numbered Vector Space
-}
complexVectorSpace : VectorSpace (ComplexNumbers.ComplexNumber Float)
complexVectorSpace =
    { abelianGroup = complexAbelianGroup
    , vectorScalarMultiplication = scalarMultiplication ComplexNumbers.field
    , field = ComplexNumbers.field
    }


{-| Real Numbered Inner Product Space
-}
realInnerProductSpace : InnerProductSpace (Real.Real Float)
realInnerProductSpace =
    { vectorSpace = realVectorSpace
    , innerProduct = dotProduct Real.field
    , length = lengthReal
    , distance = distanceReal
    }


{-| Complex Numbered Inner Product Space
-}
complexInnerProductSpace : InnerProductSpace (ComplexNumbers.ComplexNumber Float)
complexInnerProductSpace =
    { vectorSpace = complexVectorSpace
    , innerProduct = dotProduct ComplexNumbers.field
    , length = lengthComplex
    , distance = distanceComplex
    }
