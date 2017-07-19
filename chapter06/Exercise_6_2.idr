import Data.Vect

{-1-}
Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

{-2-}
data Format = Number Format
            | Str Format
            | Chr Format
            | Dbl Format
            | Lit String Format
            | End
%name Format fmt

PrintfType : Format -> Type
PrintfType (Number fmt) = (i : Int) -> PrintfType fmt
PrintfType (Str fmt) = (str : String) -> PrintfType fmt
PrintfType (Chr fmt) = (chr : Char) -> PrintfType fmt
PrintfType (Dbl fmt) = (dbl : Double) -> PrintfType fmt
PrintfType (Lit x fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt: Format) -> (acc : String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Chr fmt) acc = \chr => printfFmt fmt (acc ++ cast chr)
printfFmt (Dbl fmt) acc = \dbl => printfFmt fmt (acc ++ show dbl)
printfFmt (Lit x fmt) acc = printfFmt fmt (acc ++ x)
printfFmt End acc = acc

toFormat : (xs : List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dbl (toFormat chars)
toFormat (c :: chars) = case toFormat chars of
                             Lit lit chars' => Lit (strCons c lit) chars'
                             fmt => Lit (cast c) fmt

printf : (fmt : String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

{-3-}
TupleVect : Nat -> Type -> Type
TupleVect Z x = ()
TupleVect (S k) x = (x, (TupleVect k x))

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
