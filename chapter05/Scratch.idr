module Main

import System

printLength : IO ()
printLength = getLine >>= \input => let len = length input in
                                        putStrLn (show len)

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do Just num1 <- readNumber | Nothing => pure Nothing
     Just num2 <- readNumber | Nothing => pure Nothing
     pure (Just (num1, num2))

countdown : (secs: Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown (S secs) = do putStrLn (show (S secs))
                        usleep 1000000
                        countdown secs

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")
