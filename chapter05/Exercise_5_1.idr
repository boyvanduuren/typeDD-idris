printLonger : IO ()
printLonger = do putStr "First string: "
                 first <- getLine
                 let lenFirst = length first
                 putStr "Second string: "
                 second <- getLine
                 let lenSecond = length second
                 let longest = if lenFirst > lenSecond then lenFirst else lenSecond
                 putStrLn ("Length of longest input is: " ++ show longest)

printLonger2 : IO ()
printLonger2 = putStr "First string: " >>= \_ =>
               getLine >>= \first =>
               let lenFirst = length first in
               putStr "Second string: " >>= \_ =>
               getLine >>= \second =>
               let lenSecond = length second
                   longest = if lenFirst > lenSecond then lenFirst else lenSecond in
               putStrLn ("Length of longest input is: " ++ show longest)
