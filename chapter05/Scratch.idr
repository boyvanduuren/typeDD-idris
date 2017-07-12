module Main

import System
import Data.Vect

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

data VectUnknown : Type -> Type where
  MkVect : (len : Nat) -> Vect len a -> VectUnknown a

readVect : IO (len ** Vect len String)
readVect = do
  x <- getLine
  if (x == "")
    then pure (_ ** [])
    else do (_ ** xs) <- readVect
            pure (_ ** x :: xs)

printVect : Show a => VectUnknown a -> IO ()
printVect (MkVect len xs) =
  putStrLn (show xs ++ " (length "  ++ show len ++ ")")

zipInputs : IO ()
zipInputs = do
  putStrLn "Enter first vector: "
  (len1 ** vec1) <- readVect
  putStrLn "Enter second vector: "
  (len2 ** vec2) <- readVect
  case exactLength len1 vec2 of
    Nothing => putStrLn "Vectors are of different lengths"
    Just vec2' => printLn (zip vec1 vec2')

main : IO ()
main = do
  putStr "Enter your name: "
  x <- getLine
  putStrLn ("Hello " ++ x ++ "!")
