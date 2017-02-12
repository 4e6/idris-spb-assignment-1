module Main

import Data.Vect

infixr 5 .+.

data Schema = SString                -- String component
            | SInt                   -- Int Component
            | (.+.) Schema Schema    -- Combination of schemas (use tuples)

SchemaType : Schema -> Type
-- Calculates type for the datastore elements
-- e.g. (String, String, Int) from (SString .+. (SString .+. SInt))

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
-- setting schema should be supported only if the store is empty, 
-- otherwise return Nothing

-- User commands
data Command : Schema -> Type where
     SetSchema : Schema -> Command schema
     Add : SchemaType schema -> Command schema
     Get : Integer -> Command schema
     Quit : Command schema


parsePortion : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
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
-- invent your own schema string representation and parse it

parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
-- invent your own syntax for user input and parse it

display : SchemaType schema -> String
-- pattern match over schema implicit argument to get information about types
-- and how to display them

getEntry : (pos : Integer) -> (store : DataStore) ->
           Maybe (String, DataStore)
-- use integerToFin and index to extract data out of the store

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " ?processInput
