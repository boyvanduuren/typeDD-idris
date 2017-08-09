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

display : SchemaType schema -> String
display {schema = SString} item = item
display {schema = SInt} item = show item
display {schema = (x .+. y)} (iteml, itemr) = display iteml ++ ", " ++ display itemr

getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store = let store_items = items store in
                               (case integerToFin pos (size store) of
                                     Nothing => Just ("Out of range\n", store)
                                     (Just x) => Just (display (index x store_items) ++ "\n", store))


data Command : Schema -> Type where
  SetSchema : Schema -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Quit : Command schema

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input)
  where
    getQuoted : List Char -> Maybe (String, String)
    getQuoted ('"' :: xs) = case span (/= '"') xs of
                                 (quoted, '"' :: rest) => Just (pack quoted, ltrim (pack rest))
                                 _ => Nothing
    getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                              ("", rest) => Nothing
                              (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schemal .+. schemar) input = case parsePrefix schemal input of
                                               Nothing => Nothing
                                               Just (l_val, input') =>
                                                case parsePrefix schemar input' of
                                                  Nothing => Nothing
                                                  Just (r_val, input'') =>
                                                    Just ((l_val, r_val), input'')

parseBySchema : (schema : Schema) -> String -> Maybe (SchemaType schema)
parseBySchema schema input = case parsePrefix schema input of
                                  Nothing => Nothing
                                  (Just (res, "")) => Just res
                                  Just _ => Nothing

parseSchema : List String -> Maybe Schema
parseSchema ("String" :: xs) = case xs of
                                    [] => Just SString
                                    _ => case parseSchema xs of
                                      Nothing => Nothing
                                      Just xs_schema => Just (SString .+. xs_schema)
parseSchema ("Int" :: xs) = case xs of
                                    [] => Just SInt
                                    _ => case parseSchema xs of
                                      Nothing => Nothing
                                      Just xs_schema => Just (SInt .+. xs_schema)
parseSchema _ = Nothing

setSchema : (store : DataStore) -> Schema -> Maybe DataStore
setSchema store schema = case size store of
                              Z => Just (MkData schema _ [])
                              (S k) => Nothing

parseCommand : (schema : Schema) -> (cmd : String) -> (args : String) ->
  Maybe (Command schema)
parseCommand _ "schema" rest = case parseSchema (words rest) of
                                    Nothing => Nothing
                                    (Just schema) => Just (SetSchema schema)
parseCommand schema "add" rest = case parseBySchema schema rest of
                                      Nothing => Nothing
                                      (Just rest_ok) => Just (Add rest_ok)
parseCommand schema "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand schema "quit" _ = Just Quit
parseCommand _ _ _ = Nothing

parse : (schema : Schema) -> (input: String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
                   (cmd, args) => parseCommand schema cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store input = case parse (schema store) input of
                                Nothing => Just ("Invalid command\n", store)
                                (Just (SetSchema schema')) => (case setSchema store schema' of
                                                                    Nothing => Just ("Can't create new schema in non-empty datastore\n", store)
                                                                    (Just store') => Just ("New schema created\n", store'))
                                (Just (Add x)) => Just ("ID " ++ show (size store) ++ "\n",
                                                       (addToStore store x))
                                (Just (Get x)) => getEntry x store
                                (Just Quit) => Nothing

main : IO ()
main = replWith (MkData (SString .+. SString .+. SInt) _ []) "Command: " processInput