import Prelude.Nat
import Data.Vect

-- Use implicit arguments to implement following function
vlength : Vect n a -> Nat
vlength {n} _ = n


-- sum of vectors (define recursively)
vadd : Num a => Vect n a -> Vect n a -> Vect n a
vadd [] [] = []
vadd (x :: xs) (y :: ys) = x + y :: vadd xs ys


-- scalar product (use functions zipWith and sum)
vScalarProd : Num a => Vect n a -> Vect n a -> a
vScalarProd xs ys = sum $ zipWith (*) xs ys


-- replace all coordinates that are close to zero with zero
toAxis : (eps : Double) -> Vect n Double -> Vect n Double
toAxis eps = map round'
  where
    round' x = if (x < eps) then 0 else x

-- increase vector dimension, adding 0 as the first coordinate
incDimFirst_plusN1 : (xs : Vect n Double) -> Vect (n+1) Double
incDimFirst_plusN1 {n} xs =
  rewrite plusCommutative n 1 in 0 :: xs

incDimFirst : Vect n Double -> Vect (n+1) Double
incDimFirst xs = incDimFirst_plusN1 xs


-- increase vector dimension, adding 0 as the last coordinate
incDimLast : Vect n Double -> Vect (n+1) Double
incDimLast xs = incDimLast' 0.0 xs
  where
    incDimLast' : (x : Double) -> (xs : Vect n Double) -> Vect (n+1) Double
    incDimLast' x [] = [x]
    incDimLast' x (y :: ys) = y :: incDimLast' x ys

incDimLast' : Vect n Double -> Vect (n+1) Double
incDimLast' xs = xs ++ [0]

-- project vector to the space given by vector of indices
-- use function `map`
project : Vect n a -> Vect k (Fin n) -> Vect k a
project {n} xs ys = map takeInd ys
  where
    takeInd : Fin n -> a
    takeInd x = index x xs

project' : Vect n a -> Vect k (Fin n) -> Vect k a
project' xs = map (flip index xs)

test1 : Bool
test1 = project [1,2,3,4] [FS FZ, FZ, FS (FS FZ)] == [2,1,3]
        && project [1,2,3,4] [FS FZ, FS FZ, FS (FS (FS FZ))] == [2,2,4]
        && project [0] [FZ, FZ, FZ, FZ] == [0,0,0,0]
--      Following tests don't compile, why Reasons differ!
--      && project [1,2,3,4] [FS FZ, FS FZ, (FS (FS (FS (FS FZ))))] == [2,2,0]
--      && project [0] [FZ, FZ, FZ, FZ] == [0,0,0]

-- reverse given vector
reverse' : Vect n a -> Vect n a
reverse' [] = []
reverse' xs = go [] xs
  where
    go_jZ : (acc : Vect j a) -> Vect (plus j 0) a
    go_jZ {j} acc = rewrite plusCommutative j 0 in acc

    plusSS : (j : Nat) -> (len : Nat) -> (plus (S j) len) = plus j (S len)
    plusSS Z len = Refl
    plusSS (S k) len = rewrite plusSS k len in Refl

    go : (acc : Vect j a) -> (xs : Vect k a) -> Vect (j+k) a
    go {j} {k = Z}   acc []        = go_jZ acc
    go {j} {k = S k} acc (x :: xs) = rewrite sym $ plusSS j k in go (x::acc) xs


-- matrix transposition
transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
-- transpose_mat xs = sequence xs
transpose_mat {n = Z}     [] = []
transpose_mat {n = (S k)} [] = [] :: transpose_mat []
transpose_mat (xs :: yss)    = zipWith (::) xs (transpose_mat yss)
-- use zipWith

-- matrix addition and multiplication

addMatrix : Num numType => Vect rows (Vect cols numType)
                        -> Vect rows (Vect cols numType)
                        -> Vect rows (Vect cols numType)
addMatrix xs ys = zipWith addMatrix_op xs ys
  where
    addMatrix_op : Vect cols numType -> Vect cols numType -> Vect cols numType
    addMatrix_op xs ys = zipWith (+) xs ys

multMatrix : Num numType => Vect n (Vect m numType)
                         -> Vect m (Vect p numType)
                         -> Vect n (Vect p numType)
multMatrix xs ys = map (multMatrix_row_op (transpose_mat ys)) xs
  where
    multMatrix_row_op : (ys : Vect p (Vect m numType)) -> Vect m numType -> Vect p numType
    multMatrix_row_op xs ys = map (multMatrix_mul_op ys) xs
      where
        multMatrix_mul_op : (ys : Vect m1 numType) -> Vect m1 numType -> numType
        multMatrix_mul_op xs ys = sum $ zipWith (*) xs ys


test_multMatrix : Bool
test_multMatrix =
  multMatrix [[1,2,3],[4,5,6]] [[7,8],[9,10],[11,12]] == [[58, 64], [139, 154]]
