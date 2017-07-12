import Data.Vect

{-1-}
readToBlank : IO (List String)
readToBlank = do
  x <- getLine
  if (x == "")
    then pure []
    else do xs <- readToBlank
            pure (x :: xs)

{-2-}
readAndSave : IO ()
readAndSave = do
  strings <- readToBlank
  putStr "Enter filename: "
  filename <- getLine
  Right () <- writeFile filename (unlines strings)
    | Left err => putStrLn (show err)
  pure ()


{-3-}
readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
        Right file <- openFile filename File.Read
          | Left err => pure (_ ** [])
        Right contents <- getVectFromFile file
          | Left err => pure (_ ** [])
        pure contents
  where getVectFromFile : (file : File) -> IO (Either FileError (n ** Vect n String))
        getVectFromFile file = do
          Right line <- fGetLine file
            | Left err => pure (Left err)
          eof <- fEOF file
          if eof
            then pure (Right (_ ** []))
            else do Right (_ ** rest) <- (getVectFromFile file)
                    pure (Right (_ ** line :: rest))
