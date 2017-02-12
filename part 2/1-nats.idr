-- Define function `even` for determining if the given natural number is even
even : Nat -> Bool


-- Should be True
test1 : Bool
test1 = even 2 && even (S (S (S (S (S (S (S (S (S (S (S (S Z))))))))))))

{-
Define recursive function, which is evaluated according to the following rules:
               |  0,     if a < b
     a .-. b = | 
               |  a - b, otherwise
-}

infixl 10 .-.

(.-.) : Nat -> Nat -> Nat
n .-. m = ?rhs

-- Should be True
test2 : Bool
test2 = (5 .-. 2 == 3) && (2 .-. 5 == 0)

{-
   Define function `abs'` for calculating absolute value of the difference between a b using (.-.):
     |a - b| = (a .-. b) + (b .-. a)
-}

abs' : Nat -> Nat -> Nat


-- Should be True
test3 : Bool
test3 = abs' 2 5 == 3

{-
  Give another definition of `plus`, which is based on the pattern matching
  over the second argument.
-}

plus'' : Nat -> Nat -> Nat


-- Should be True
test4 : Bool
test4 = (plus'' 2 5 == 7) && (plus'' 5 2 == 7)

all_tests : Bool
all_tests = test1 && test2 && test3 && test4
