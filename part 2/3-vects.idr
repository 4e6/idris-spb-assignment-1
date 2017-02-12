import Data.Vect

-- Use implicit arguments to implement following function
vlength : Vect n a -> Nat


-- sum of vectors (define recursively)
vadd : Num a => Vect n a -> Vect n a -> Vect n a


-- scalar product (use functions zipWith and sum)
vScalarProd : Num a => Vect n a -> Vect n a -> a


-- replace all coordinates that are close to zero with zero
toAxis : (eps : Double) -> Vect n Double -> Vect n Double


-- increase vector dimension, adding 0 as the first coordinate
incDimFirst : Vect n Double -> Vect (n+1) Double


-- increase vector dimension, adding 0 as the last coordinate
incDimLast : Vect n Double -> Vect (n+1) Double


-- project vector to the space given by vector of indices
-- use function `map`
project : Vect n a -> Vect k (Fin n) -> Vect k a


test1 : Bool
test1 = project [1,2,3,4] [FS FZ, FZ, FS (FS FZ)] == [2,1,3]
        && project [1,2,3,4] [FS FZ, FS FZ, FS (FS (FS FZ))] == [2,2,4] 
        && project [0] [FZ, FZ, FZ, FZ] == [0,0,0,0]
--      Following tests don't compile, why? Reasons differ!
--      && project [1,2,3,4] [FS FZ, FS FZ, (FS (FS (FS (FS FZ))))] == [2,2,0] 
--      && project [0] [FZ, FZ, FZ, FZ] == [0,0,0]

-- reverse given vector
reverse' : Vect n a -> Vect n a

-- matrix transposition
transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
-- use zipWith

-- matrix addition and multiplication
addMatrix : Num numType => Vect rows (Vect cols numType) 
                           -> Vect rows (Vect cols numType) 
                           -> Vect rows (Vect cols numType)

multMatrix : Num numType => Vect n (Vect m numType) 
                            -> Vect m (Vect p numType) 
                            -> Vect n (Vect p numType)
