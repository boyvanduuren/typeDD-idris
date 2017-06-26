data BSTree : Type -> Type where
     Empty: Ord elem => BSTree elem
     Node : Ord elem => (left : BSTree elem) -> (val : elem) ->
                        (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case compare x val of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)

{-1-}
listToTree : Ord a => List a -> BSTree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)

{-2-}
treeToList : BSTree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ [val] ++ treeToList right

{-3-}
data Expr = Val Integer
          | Sum Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

{-4-}
evaluate : Expr -> Integer
evaluate (Val x) = x
evaluate (Sum x y) = evaluate x + evaluate y
evaluate (Sub x y) = evaluate x - evaluate y
evaluate (Mult x y) = evaluate x * evaluate y

{-5-}
maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing (Just x) = Just x
maxMaybe (Just x) Nothing = Just x
maxMaybe (Just x) (Just y) = case compare x y of
                                  LT => Just y
                                  EQ => Just x
                                  GT => Just x

{-6-}
data Shape = Triangle Double Double
           | Rectangle Double Double
           | Circle Double

area : Shape -> Double
area (Triangle x y) = (x*y)/2
area (Rectangle x y) = x*y
area (Circle x) = pi * x * x

data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive t@(Triangle x y)) = Just (area t)
biggestTriangle (Primitive _) = Nothing
biggestTriangle (Combine pict pict1) =
  maxMaybe (biggestTriangle pict) (biggestTriangle pict1)
biggestTriangle (Rotate _ pict) = biggestTriangle pict
biggestTriangle (Translate _ _ pict) = biggestTriangle pict

testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))
testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))
