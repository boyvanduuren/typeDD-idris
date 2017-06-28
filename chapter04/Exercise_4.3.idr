module Main

import Data.Vect

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                               (case integerToFin pos (size store) of
                                     Nothing => Just ("Out of range\n", store)
                                     (Just x) => Just (index x store_items ++ "\n", store))

{-2,3-}
search : Nat -> String -> Vect n String -> String
search idx x [] = ""
search idx x (y :: xs) = case isInfixOf x y of
                              False => "" ++ search (S idx) x xs
                              True => show idx ++ ": " ++ y ++ "\n" ++
                                      search (S idx) x xs
{-end-}

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
                                                       (addToStore store x))
                                (Just (Get x)) => getEntry x store
                                (Just Size) => Just (show (size store) ++ "\n", store)
                                (Just (Search x)) =>
                                  Just (search 0 x (items store), store)
                                (Just Quit) => Nothing

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
