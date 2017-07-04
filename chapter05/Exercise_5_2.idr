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
