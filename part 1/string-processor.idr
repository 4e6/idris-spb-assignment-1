import Functions

{-
  Write a complete repl-program which follows given protocol:
    * every input string starts with either P or C with one space after it
    * process rest of the string using `palindrome` or `counts` respectively
    
  Example session:
  > P noon
  noon is a palindrome
  > C Hello, Idris world!
  (3, 19)
  > P midday
  midday is not a palindrome
  
  Hints: 
    a) you may find functions `take`, `drop`, `words`, `unwords`, `substr`
       useful (make sure you understand what these functions do);
    b) try to implement as many auxiliary functions as you can.
-}

main : IO ()
main = putStrLn "This is unimplemented interactive string processor"
