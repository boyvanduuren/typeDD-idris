occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item [] = 0
occurrences item (x :: xs) = (case item == x of
                                  False => 0
                                  True => 1) + occurrences item xs

data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid Solid = True
  (==) Liquid Liquid = True
  (==) Gas Gas = True
  (==) _ _ = False

  (/=) x y = not (x == y)

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty Empty = True
  (==) (Node left e right) (Node left' e' right') =
    left == left' && e == e' && right == right'
  (==) _ _ = False

record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

Eq Album where
  (==) (MkAlbum artist title year) (MkAlbum artist' title' year') =
    artist == artist' && title == title' && year == year'

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year') =
    case compare artist artist' of
          EQ => case compare year year' of
                      EQ => compare title title' 
                      diff_year => diff_year
          diff_artist => diff_artist

help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]

-- 7.3
Functor Tree where
  map func Empty = Empty
  map func (Node left e right)
    = Node (map func left)
           (func e)
           (map func right)

Foldable Tree where
  foldr func acc Empty = acc
  foldr func acc (Node left e right) =
    let leftfold = foldr func acc left
        rightfold = foldr func leftfold right in
        func e rightfold