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

data Action : Type where
  P : Action
  C : Action

fromChar' : Char -> Maybe Action
fromChar' 'P' = Just P
fromChar' 'C' = Just C
fromChar' _   = Nothing

parseInput : (s : String) -> Maybe (Action, String)
parseInput s = case length s > 0 of
  False => Nothing
  True => (\x => (x, trim (strTail s))) <$> fromChar' (strHead s)

main : IO ()
main = do
  s <- getLine
  putStrLn $ case parseInput s of
    Nothing => "Invalid input. Exiting ..."
    (Just (action, input)) => case action of
      P => case palindrome input of
        False => input ++ " is not a palindrome"
        True => input ++ " is a palindrome"
      C => show $ counts input
