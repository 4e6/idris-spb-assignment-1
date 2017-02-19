import Functions

{-
  Write a complete program which prompts for an input, calls
  the function `counts` and prints its output.
-}



main : IO ()
main = do
  putStrLn "Input words"
  s <- getLine
  print $ counts s
