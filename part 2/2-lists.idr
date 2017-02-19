||| Calculates length of the given list
length' : List a -> Nat
length' [] = 0
length' (x :: xs) = 1 + length xs

||| Returnes reversed list
reverse' : List a -> List a
reverse' [] = []
reverse' (x :: xs) = (reverse' xs) ++ [x]

||| Calculates sum of the fist and the last elements of the given list
||| or returns 0 if the list is empty
sum_fl : List Integer -> Integer
sum_fl [] = 0
sum_fl (x :: xs) = x + sum_fl' xs
  where
    sum_fl' []        = 0
    sum_fl' (x :: []) = x
    sum_fl' (x :: xs) = sum_fl' xs

||| Returns length of the given list if there exists
||| 0 in there or returns (-1) otherwise
len_withZero : List Integer -> Int
len_withZero xs = if any (== 0) xs
  then cast $ Prelude.List.length xs
  else -1

-- Use `filter` and `length`.

{-
 Define datatype with four possible values corresponding to various
 list calculations:
   * sum
   * product
   * maximum
   * minimum

 Define function, which processes list according to the specified
 calculation. If list is empty result should be Nothing.
-}

data Calculation : Type where
  Sum     : Calculation
  Product : Calculation
  Maximum : Calculation
  Minimum : Calculation

fromString : String -> Maybe Calculation
fromString "SUM"  = Just Sum
fromString "PROD" = Just Product
fromString "MAX"  = Just Maximum
fromString "MIN"  = Just Minimum
fromString _      = Nothing

process_list : Calculation -> (l: List Integer) -> Maybe Integer
process_list _ [] = Nothing
process_list Sum l = Just $ sum l
process_list Product l = Just $ product l
process_list Maximum l = Prelude.List.head' $ sortBy (flip compare) l
process_list Minimum l = Prelude.List.head' $ sort l

{-
 Use previously defined function and datatype in the case when input data
 are given in a string (first word in the string can be either SUM, PROD,
 MIN, or MAX). Define whatever auxiliary functions you need.
 Your should return Nothing if the given string is somehow incorrect.

 Hint: functions `map` and `cast` may be useful.

 Recommendation: don't try to overcomplicate this function checking what
 exactly follows the first word, just cast it to an Integer without doubt.
-}

process_string : String -> Maybe Integer
process_string s = do
  let xs = split (== ' ') s
  h <- Prelude.List.head' xs
  op <- fromString h
  t <- Prelude.List.tail' xs
  process_list op (cast <$> t)

-- Should be True
test : Bool
test = (process_string "SUM" == Nothing)
       && (process_string "SUM 1 2 3" == Just 6)
       && (process_string "MAX 5 2 3" == Just 5)
       && (process_string "PROD 1 0 5" == Just 0)
       && (process_string "MIN 1 0 5" == Just 0)
