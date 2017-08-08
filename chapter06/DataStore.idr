module Main

import Data.Vect

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

{-
data DataStore : Type where
  MkData : (schema : Schema) ->
           (size : Nat) ->
           (items : Vect size (SchemaType schema)) ->
           DataStore
-}

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                               (case integerToFin pos (size store) of
                                     Nothing => Just ("Out of range\n", store)
                                     (Just x) => Just (?display (index x store_items) ++ "\n", store))

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" _ = Just Size
parseCommand "quit" _ = Just Quit
parseCommand _ _ = Nothing

parse : (input: String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse input of
                                Nothing => Just ("Invalid command\n", store)
                                (Just (Add x)) => Just ("ID " ++ show (size store) ++ "\n",
                                                       (addToStore store (?convert x)))
                                (Just (Get x)) => getEntry x store
                                (Just Size) => Just (show (size store) ++ "\n", store)
                                (Just (Search x)) =>
                                  Just (search 0 x (items store), store)
                                (Just Quit) => Nothing
