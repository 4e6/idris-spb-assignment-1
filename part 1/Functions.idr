module Functions

%access export

{-
  1) Determine types of the following values without referring to REPL
  and then check your ideas with ":t". Don't forget to mention your 
  mistakes if any.
  
  1) ("A", "B", "C") : ???
  2) ["A", "B", "C"] : ???
  3) ["A", "B", 'C'] : ???
  4) ("A", "B", 'C') : ???
  5) (("A", "B"), 'C') : ???
  6) ("A", ("B", 'C')) : ???
  7) [["A", "B"], ["C"]] : ???
  8) [("A", "B"), "C"] : ???
-}

{-
  2) Implement case-insensitive function `palindrome` using 
  library functions reverse and toLower. Make sure your 
  implementation passes tests given.
-}

palindrome : String -> Bool


test_palindrome : String
test_palindrome =  if palindrome "pullup" 
                     && palindrome "Kayak"
                     && palindrome "noON"
                     && palindrome "Glenelg"
                     && palindrome "tattArratTat"
                     && palindrome "kuulilennuteetunneliluuk"
                     && not (palindrome "Idris")
                   then "Tests passed"
                   else "Tests failed"

-- Btw, do you know meanings of test words?

{-
  3) Write function `counts`, which returns a pair of the number of words 
  in the input and the number of characters in the input.  For example, 
  the input "Hello, Idris world!" should give the output (3, 19) .
  Provide tests for this function following the idea from previous exercise.
-}

counts : String -> (Nat, Nat)

{-
  4) Write function `top_ten`, which returns ten largest values in a list. 
  Hint: You may find functions `take` and `sort` useful (use :t and :doc for details). 
  Provide tests.
-}

top_ten: Ord a => List a -> List a

{-
  5) Write function `over_length`, which returns the number of strings in the list
  longer than the given number of characters. For example, evaluating 
     over_length 3 ["One", "Two", "Three", "Four"] 
  should give the output 2.
  Provide tests.
-}

over_length : Nat -> List String -> Nat
