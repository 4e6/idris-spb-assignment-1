module Main

import Data.Vect

infixr 5 .+.

data Schema = SString                -- String component
            | SInt                   -- Int Component
            | SBool
            | (.+.) Schema Schema    -- Combination of schemas (use tuples)

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType SBool = Bool
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

-- Calculates type for the datastore elements
-- e.g. (String, String, Int) from (SString .+. (SString .+. SInt))

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) stype =
  MkData schema (S size) (stype :: items)

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema (MkData _ Z     _) x = Just $ MkData x Z []
setSchema (MkData _ (S k) _) _ = Nothing
-- setting schema should be supported only if the store is empty,
-- otherwise return Nothing

clearItems : (store : DataStore) -> DataStore
clearItems (MkData schema size items) = MkData schema _ []

-- User commands
data Command : Schema -> Type where
     SetSchema : Schema -> Command schema
     Add : SchemaType schema -> Command schema
     GetContents : Command schema
     Get : Integer -> Command schema
     Clear : Command schema
     Quit : Command schema

splitFirst : (Char -> Bool) -> String -> Maybe (String, String)
splitFirst f s with (break f (unpack s))
  splitFirst f _ | (as, []) = Nothing
  splitFirst f _ | (as, x::xs) =
    let as' = trim $ pack as
        bs' = ltrim $ pack xs
    in if length bs' > 0 then Just (as', bs') else Nothing

parsePortion : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePortion SString x = Just (x, "")
parsePortion SInt x =
  let (h,t) = span isDigit x
  in if h == "" then Nothing else Just (cast h, t)
parsePortion SBool x = case x of
                            "T" => Just (True, "")
                            "F" => Just (False, "")
                            _ => Nothing
parsePortion (y .+. z) x = case splitFirst (== '+') x of
  Nothing => Nothing
  (Just (a, b)) => case (parsePortion y a, parsePortion z b) of
    (Just (s1, ""), Just (s2, "")) => Just ((s1, s2), "")
    _ => Nothing

-- This function tries to parse one element of a schema (string or integer or pair)
-- in one recursive step, it is the harderst function to implement.
-- Second component of the pair is what was left unparsed.
-- Use span, pack, ltrim, cast

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema x = case parsePortion schema x of
                              Nothing => Nothing
                              Just (res, "") => Just res -- returns Just only if
                                                         -- everything was parsed
                              Just _ => Nothing

parseSchema : String -> Maybe Schema
parseSchema "String" = Just SString
parseSchema "Int"    = Just SInt
parseSchema "Bool"   = Just SBool
parseSchema x with (splitFirst (== '+') x)
  parseSchema x | Nothing = Nothing
  parseSchema x | (Just (a, b)) = [| parseSchema a .+. parseSchema b |]

-- invent your own schema string representation and parse it

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema "Quit" = Just Quit
parse schema "Get" = Just GetContents
parse schema "Clear" = Just Clear
parse schema input with (splitFirst (== ' ') input)
  parse schema input | Nothing = Nothing
  parse schema input | (Just ("SetSchema", b)) = [| SetSchema $ parseSchema b |]
  parse schema input | (Just ("Add", b))  = [| Add $ parseBySchema schema b |]
  parse schema input | (Just ("Get", b))  = if all isDigit (unpack b)
                                            then Just $ Get (cast b)
                                            else Nothing
  parse schema input | (Just _) = Nothing

-- invent your own syntax for user input and parse it

display : SchemaType schema -> String
display {schema = SString} x = x
display {schema = SInt} x = show x
display {schema = SBool} x = show x
display {schema = (y .+. z)} (a, b) = "(" ++ display a ++ "," ++ display b ++ ")"
-- pattern match over schema implicit argument to get information about types
-- and how to display them

getEntry : (pos : Integer)
         -> (store : DataStore)
         -> Maybe (String, DataStore)
getEntry pos store with (integerToFin pos (size store))
  getEntry pos store | Nothing = Nothing
  getEntry pos store | (Just x) =
    let v = index x (items store)
    in Just (display v, store)

-- use integerToFin and index to extract data out of the store

showError : (msg : String) -> String
showError msg = "Error: " ++ msg ++ "!\n"

showOk : (msg : String) -> String
showOk msg = "Ok " ++ msg ++ "\n"

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store s with (parse (schema store) (trim s))
  processInput store s | Nothing =
    Just (showError "available commands: SetSchema, Add, Get, Clear, Quit", store)
  processInput store s | (Just (SetSchema x)) =
    case setSchema store x of
      Nothing => Just (showError "wrong schema", store)
      (Just store') => Just (showOk "", store')
  processInput store s | (Just (Add x)) =
    let store' = addToStore store x
    in Just (showOk "", store')
  processInput store s | (Just GetContents) =
    let vec = items store
        vals = intersperse ", " $ map display vec
        str = foldr (++) "" vals
    in Just (showOk $ "[" ++ str ++ "]", store)
  processInput store s | (Just (Get i)) =
    case getEntry i store of
      Nothing => Just (showError "wrong index", store)
      (Just (a, b)) => Just (showOk a, b)
  processInput store s | (Just Clear) =
    Just (showOk "Store cleared", clearItems store)
  processInput store s | (Just Quit) = Nothing


main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput
