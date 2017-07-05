import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

{-1,3-}
guess : (tries: Nat) -> (target : Nat) -> IO ()
guess tries target = do
  putStr ("guess [tries: " ++ (show tries) ++ "]: ")
  Just guessed <- readNumber
    | Nothing => do putStrLn "Invalid"
                    guess (S tries) target
  if guessed == target then putStrLn "Correct"
  else
    if guessed < target then
      do putStrLn "Too low, guess again"
         guess (S tries) target
    else
      do putStrLn "Too high, guess again"
         guess (S tries) target

{-2-}
randomGuess : IO ()
randomGuess = do
  t <- time
  guess 0 (cast (t `mod` 101))

{-4-}
||| Test function for ownRepl
test : String -> String
test x = let maybeHead : Maybe String = listToMaybe (words x) in
         case maybeHead of
           Nothing => ""
           Just s => s

||| Test function for ownReplWith
test2 : String -> String -> Maybe (String, String)
test2 _ "z" = Nothing
test2 x y = let new = x ++ y in
  Just (new, new)

ownRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
ownRepl prompt onInput = do
  putStr prompt
  input <- getLine
  putStr (onInput input)
  ownRepl prompt onInput

ownReplWith : (state : a) -> (prompt : String) ->
              (onInput : a -> String -> Maybe (String, a)) -> IO ()
ownReplWith state prompt onInput = do
  putStr prompt
  input <- getLine
  case onInput state input of
    Nothing => pure ()
    Just (out, newState) => do putStr out
                               ownReplWith newState prompt onInput
