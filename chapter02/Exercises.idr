module Main

||| Returns whether a string is a palindrome of a minimum length.
||| @ len The length of the palindrome should be greater than len.
||| @ str The string to check.
isPalindrome : (len: Nat) -> (str: String) -> Bool
isPalindrome len str = if length str > len
  then let strL = toLower str in
           strL == reverse strL
  else False

||| Given a string return how a tuple of the amount of words and the
||| amount of symbols.
counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

||| Return the ten largest values in a list, sorted from large to small.
top_ten : Ord a => List a -> List a
top_ten l = take 10 (reverse (sort l))

||| Given a list of strings return how many are over a given length.
||| @ len If a word is to be counted its length needs to be larger than len.
||| @ strs The list of strings to check.
over_length: (len: Nat) -> (strs: List String) -> Nat
over_length len strs = length (filter (\str => length str > len) strs)

main : IO ()
main = repl "palindrome> " (\str => show (isPalindrome 5 str) ++ "\n")
--main = repl "count> " (\str => show (counts str) ++ "\n")
